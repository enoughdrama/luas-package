local math = {min = math.min, max = math.max, abs = math.abs, fmod = math.fmod, floor = math.floor, sqrt = math.sqrt, sin = math.sin, cos = math.cos, rad = math.rad, random = math.random, pow = math.pow}
local f = {ipairs = ipairs, assert = assert, pairs = pairs, pcall = pcall, error = error, next = next, tostring = tostring, unpack = unpack}
local table = {sort = table.sort, remove = table.remove, concat = table.concat, insert = table.insert, unpack  = table.unpack}
local string = {match = string.match, gsub = string.gsub, format = string.format}


local mindmg = {
	name = 'mindmg',
	ref = {'Rage', 'Other', 'Minimum damage override'}
}

local surface = require 'gamesense/surface'
local csgo_weapons = require 'gamesense/csgo_weapons'
local vector = require 'vector'
local ffi = require 'ffi'
local bit = require 'bit'

local font = surface.create_font('Verdana', 26, 700, {0x010})
local bomb_font = surface.create_font('Verdana', 28, 700, {0x010})

local menu = {
	ind = 'Indicators',
	n_ind = 'Indicators (new)',
	fl = 'Fake lag indicator',
	dt = 'Increase double tap speed',
	baim = 'Baim if lethal', 
	aa = 'Legit-AA on E key',
	legf = 'Leg fucker',
	get = {}
} menu.multiselect = ui.new_multiselect('AA', 'Other', 'Utilities', {menu.ind, menu.n_ind, menu.fl, menu.dt, menu.baim, menu.aa, menu.legf})

local scrsize_x, scrsize_y = client.screen_size()
local center_x, center_y = scrsize_x / 2, scrsize_y / 2
local client_log = client.log
local client_draw_text = client.draw_text
local client_screensize = client.screen_size
local client_set_event_callback = client.set_event_callback
local clrpick = ui.new_color_picker("AA", "Anti-aimbot angles", "Hotkey indicator color")
local clrpick1 = ui.new_color_picker("AA", "Anti-aimbot angles", "Hotkey indicator color1")
local _, body_yaw_slider = ui.reference("AA","Anti-aimbot angles","Body yaw")

local refs = {
	safepoint = ui.reference('Rage', 'Aimbot', 'Force safe point'),
	dmg = ui.reference('Rage', 'Aimbot', 'Minimum Damage'),
	fake_jopa = ui.reference("AA", "Anti-aimbot angles", "Body yaw"),

	duck = ui.reference('Rage', 'Other', 'Duck peek assist'),
	baim = ui.reference('Rage', 'Other', 'Force body aim'),
	dt = {ui.reference('Rage', 'Other', 'Double tap')},
	dt_fl = ui.reference('RAGE', 'Other', 'Double tap fake lag limit'),

	aa = ui.reference('AA', 'Anti-aimbot angles', 'Enabled'),
	freestand = {ui.reference('AA', 'Anti-aimbot angles', 'Freestanding')},
	fs_body_yaw = ui.reference('AA', 'Anti-aimbot angles', 'Freestanding body yaw'),
	fake_yaw = ui.reference('AA', 'Anti-aimbot angles', 'Fake yaw limit'),

	fakelag_limit = ui.reference('AA', 'Fake lag', 'Limit'),
	on_shot = {ui.reference('AA', 'Other', 'On shot anti-aim')},
	leg_m = ui.reference('AA', 'Other','Leg movement'),

	sv_maxusrcmdprocessticks = ui.reference('MISC', 'Settings', 'sv_maxusrcmdprocessticks')
}

local function contains(tab, val)
    for key, value in f.ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end

ui.set_callback(menu.multiselect, function()
	if not contains(menu.get, menu.dt) then ui.set(refs.sv_maxusrcmdprocessticks, 16) end
end)

client.set_event_callback('paint', function()
	menu.get = ui.get(menu.multiselect)
end)

local function table_remove_element(tbl, val)
	local tbl_new = {}
	for i = 1, #tbl do
		if tbl[i] ~= val then
			table.insert(tbl_new, tbl[i])
		end
	end
	return tbl_new
end

local value_prev = {}

local function on_multiselect_changed()
	local value = ui.get(menu.multiselect)

	if #value > 0 then
		if contains(value, 'Indicators') and contains(value, 'Indicators (new)') then
			local value_new = value
			if not contains(value_prev, 'Indicators') then
				value_new = table_remove_element(value_new, 'Indicators (new)')
			elseif not contains(value_prev, 'Indicators (new)') then
				value_new = table_remove_element(value_new, 'Indicators')
			end

			if contains(value_new, 'Indicators') and contains(value_new, 'Indicators (new)') then
				value_new = table_remove_element(value_new, 'Indicators')
			end

			ui.set(menu.multiselect, value_new)
			on_multiselect_changed()
			return
		end
	end

	value_prev = value
end
ui.set_callback(menu.multiselect, on_multiselect_changed)
on_multiselect_changed()

ffi.cdef[[	
	struct anim_state_halflifefan {                     char pad[ 108 ];
		float last_client_side_animation_update_time;   char pad2[ 8 ];
		float eye_yaw;                                  char pad3[ 4 ];
		float goal_feet_yaw;                            char pad4[ 32 ];
		float duck_amount;                              char pad5[ 72 ];
		float up_velocity;                              char pad6[ 48 ];
        float ground_fraction;                          char pad7[ 520 ];
        float min_yaw;
        float max_yaw;
	}
]]

local ffi_cast = ffi.cast

local function get_color_by_float(float, ...)
    local args = {...}

    local color_table_size = 0
    local color_table = {}

    for i=1, #args, 4 do
        color_table_size = color_table_size + 1

        local color_float = (i - 1) / (#args - 4)
        color_table[color_table_size] = {color_float, args[i], args[i + 1], args[i + 2], args[i + 3]}
    end
    
    local color_lower 
    local color_higher 
    local distance_lower = 1
    local distance_higher = 1
 
    for i=1, #color_table do
        local color = color_table[i]
        local color_float = color[1]
        if color_float == float then
            return color[2], color[3], color[4], color[5]
        elseif color_float > float then
            local distance = color_float - float
            if distance < distance_higher then
                color_higher = color
                distance_higher = distance
            end
        elseif color_float < float then
            local distance = float - color_float
            if distance < distance_lower then
                color_lower = color
                distance_lower = distance
            end
        end
    end

    local distance_difference = distance_lower + distance_higher
    local red = (color_lower[2] * distance_higher + color_higher[2] * distance_lower) / distance_difference
    local green = (color_lower[3] * distance_higher + color_higher[3] * distance_lower) / distance_difference
    local blue = (color_lower[4] * distance_higher + color_higher[4] * distance_lower) / distance_difference
    local alpha = (color_lower[5] * distance_higher + color_higher[5] * distance_lower) / distance_difference
 
    return red, green, blue, alpha
end

local function angle_mod(angle)
    return (360 / 65536) * bit.band(angle * (65536 / 360), 65535)
end
 
local function approach_angle(target, value, speed)
	local adjusted_speed = speed
	if adjusted_speed < 0 then
		adjusted_speed = adjusted_speed * -1
	end
 
	local angle_mod_target = angle_mod(target)
	local angle_mod_value = angle_mod(value)
 
	local delta = angle_mod_target - angle_mod_value
	if delta >= -180 then
		if delta >= 180 then
			delta = delta - 360
		end
	else
		if delta <= -180 then
			delta = delta + 360
		end
	end
 
	if delta <= adjusted_speed then
		if adjusted_speed * -1 <= delta then
			return angle_mod_target
		else
			return angle_mod_value - adjusted_speed
		end
	else
		return angle_mod_value + adjusted_speed
	end
end

local function angle_diff(dest_angle, src_angle)
	local delta = math.fmod(dest_angle - src_angle, 360)
	if dest_angle > src_angle then
		if delta >= 180 then
			delta = delta - 360
		end
	else
		if delta <= -180 then
			delta = delta + 360
		end
	end
	
	return delta
end

local function normalize_angle(angle)
    while angle > 180 do angle = angle - 360 end
    while angle < -180 do angle = angle + 360 end
    return angle
end

local entity_list = ffi.cast('void***', client.create_interface('client.dll', 'VClientEntityList003'))
local get_client_entity = ffi.cast('void*(__thiscall*)(void*, int)', entity_list[0][3])

local new_goal_feet_yaw = 0
local desync_delta = 0
local eye_yaw = 0

local function get_desync_delta()
	local send_packet = globals.chokedcommands() == 0

	local local_player = entity.get_local_player()

	local lower_body_yaw_target = entity.get_prop(local_player, 'm_flLowerBodyYawTarget')

	local local_player_ptr = ffi_cast('void***', get_client_entity(entity_list, local_player))
	local local_player_anim_state_ptr = ffi_cast('char*' , local_player_ptr) + 0x3914
	local anim_state = ffi_cast('struct anim_state_halflifefan**', local_player_anim_state_ptr)[0]

	if send_packet then
		eye_yaw = anim_state.eye_yaw
	end
	
	local last_client_side_animation_update_time_delta = math.max(globals.curtime() - anim_state.last_client_side_animation_update_time, 0)

	local local_player_velocity = vector(entity.get_prop(local_player, 'm_vecVelocity'))
	local speed = math.min(local_player_velocity:length(), 260)

	local local_player_weapon = entity.get_player_weapon(local_player)
	local local_player_weapon_item_definition_index = entity.get_prop(local_player_weapon, 'm_iItemDefinitionIndex')

	local_player_weapon = csgo_weapons[local_player_weapon_item_definition_index]

	local max_movement_speed = 260
	if local_player_weapon then
		max_movement_speed = math.max(local_player_weapon.max_player_speed, 0.001)
	end

	local running_speed = speed / (max_movement_speed * 0.52)
	local ducking_speed = speed / (max_movement_speed * 0.34)

	new_goal_feet_yaw = math.max(-360, math.min(360, new_goal_feet_yaw))

	local eye_feet_delta = angle_diff(eye_yaw, new_goal_feet_yaw)

	running_speed = math.max(0, math.min(1, running_speed))

	local yaw_modifier = (((anim_state.ground_fraction * -0.3) - 0.2) * running_speed) + 1

	if anim_state.duck_amount > 0 then
		ducking_speed = math.max(0, math.min(1, ducking_speed))
	end

	local max_yaw_modifier = yaw_modifier * anim_state.max_yaw
	local min_yaw_modifier = yaw_modifier * anim_state.min_yaw

	if eye_feet_delta <= max_yaw_modifier then
		if min_yaw_modifier > eye_feet_delta then
			new_goal_feet_yaw = math.abs(min_yaw_modifier) + eye_yaw
		end
	else
		new_goal_feet_yaw = eye_yaw - math.abs(max_yaw_modifier)
	end

	new_goal_feet_yaw = normalize_angle(new_goal_feet_yaw)

	if speed > 2 or math.abs(anim_state.up_velocity) > 100 then
		new_goal_feet_yaw = approach_angle(
			eye_yaw,
			new_goal_feet_yaw,
			((anim_state.ground_fraction * 20) + 30)
			* last_client_side_animation_update_time_delta)
	else
		new_goal_feet_yaw = approach_angle(
			lower_body_yaw_target,
			new_goal_feet_yaw,
			last_client_side_animation_update_time_delta * 100)
	end

	if send_packet then
		desync_delta = math.abs(normalize_angle(anim_state.goal_feet_yaw - new_goal_feet_yaw)) or 0
	end

	return desync_delta
end

local gamesense_color = {123, 194, 21, 255}

local function draw_indicator_circle(ctx, x, y, r, g, b, a, radius, percentage)
	client.draw_circle_outline(ctx, x, y, 0, 0, 0, 120, radius, 0, 1.0, 5)
	client.draw_circle_outline(ctx, x, y, r, g, b, a, radius - 1, 0, percentage, 3)
end

local function angle_forward(angle)
    local sin_pitch = math.sin(math.rad(angle[1]))
    local cos_pitch = math.cos(math.rad(angle[1]))
    local sin_yaw = math.sin(math.rad(angle[2]))
    local cos_yaw = math.cos(math.rad(angle[2]))

    return {cos_pitch * cos_yaw, cos_pitch * sin_yaw, -sin_pitch}
end

client.set_event_callback('setup_command',function(e)
	local weapon = entity.get_player_weapon(entity.get_local_player())

	local pitch, yaw = client.camera_angles()
    local fwd = angle_forward({pitch, yaw, 0})
	local start_pos = {client.eye_position()}
	local distance = 100
    local fraction, entindex = client.trace_line(entity.get_local_player(), start_pos[1], start_pos[2], start_pos[3], start_pos[1] + (fwd[1] * distance), start_pos[2] + (fwd[2] * distance), start_pos[3] + (fwd[3] * distance))
	
    if contains(menu.get, menu.aa) then
        if weapon ~= nil and entity.get_classname(weapon) == 'CC4' then
            if e.in_attack == 1 then
                e.in_attack = 0
                e.in_use = 1
            end
		else
			if entindex ~= -1 then
				if entity.get_classname(entindex) == 'CPropDoorRotating' then
					return
				end
			end
			if e.chokedcommands == 0 then
				e.in_use = 0
			end
		end
        ui.set(refs.fs_body_yaw, true)
	end
end)

local calc_dmg = function(local_player, player, distance)
	local weapon_ent = entity.get_player_weapon(local_player)
	if weapon_ent == nil then return end
	
	local weapon_idx = entity.get_prop(weapon_ent, "m_iItemDefinitionIndex")
	if weapon_idx == nil then return end
	
	local weapon = csgo_weapons[weapon_idx]
	if weapon == nil then return end

	local dmg_after_range = (weapon.damage * math.pow(weapon.range_modifier, (distance * 0.002))) * 1.25
	local armor = entity.get_prop(player,"m_ArmorValue")
	local newdmg = dmg_after_range * (weapon.armor_ratio * 0.5)
	if dmg_after_range - (dmg_after_range * (weapon.armor_ratio * 0.5)) * 0.5 > armor then
		newdmg = dmg_after_range - (armor / 0.5)
	end
	return newdmg
end

local fakelag_maximum, choked_ticks, choked_ticks_max, choked_ticks_prev = 14, 0, 0, 0

client.register_esp_flag("", 252, 200, 86, function(e)
    return plist.get(e, "Override prefer body aim") == "Force"
end)

client.set_event_callback('run_command', function(e)
	desync_delta = get_desync_delta()

	if contains(menu.get, menu.fl) then
		choked_ticks = e.chokedcommands
		if choked_ticks_prev >= choked_ticks or choked_ticks == 0 then
			choked_ticks_max = choked_ticks_prev
		end
		choked_ticks_prev = choked_ticks
	end

	if contains(menu.get, menu.legf) then
		--entity.set_prop(entity.get_local_player(), 'm_flPoseParameter', 1, 0)

		local rand = math.random(0,2)
		ui.set(refs.leg_m, rand == 0 and 'Always slide' or rand == 1 and 'Never slide' or 'Off')
	end

	local enemies = entity.get_players(true)
	if enemies[1] or contains(menu.get, menu.baim) or entity.is_alive(entity.get_local_player()) then 
		for i = 1, #enemies do
			local local_origin = vector(entity.get_prop(entity.get_local_player(), "m_vecAbsOrigin"))
			local distance = local_origin:dist(vector(entity.get_prop(enemies[i], "m_vecOrigin")))
			local enemy_health = entity.get_prop(enemies[i], "m_iHealth")
			local damage = calc_dmg(entity.get_local_player(), enemies[i], distance)
			local value = enemy_health <= damage and "Force" or "-"
			plist.set(enemies[i], "Override prefer body aim", value)
		end
	end



	if not contains(menu.get, menu.dt) then return end

	if ui.get(refs.dt[1]) and ui.get(refs.dt[2]) and not ui.get(refs.duck) then
		ui.set(refs.sv_maxusrcmdprocessticks, 18)
		ui.set(refs.fakelag_limit, 16)
		ui.set(refs.dt_fl, 1)
	else
		ui.set(refs.sv_maxusrcmdprocessticks, 16)
		ui.set(refs.fakelag_limit, math.min(14, ui.get(refs.fakelag_limit)))
	end
end)

local function get_custom_reference(lua, ref)
	if f.pcall(require, lua) and f.pcall(ui.reference, f.unpack(ref)) then
		local reference = {ui.reference(f.unpack(ref))}
		local ref_type = type(ui.get(reference[#reference]))

		local v_type = {
			boolean = ui.get(reference[1]) and ui.get(reference[#reference]),
			number = {ui.get(reference[#reference])}
		}

		return v_type[ref_type]
	else
		return false
	end
end

local defusers = 0
local planting_started_at
local planting_time = 3.125
local planting_color = gamesense_color
local bomb_color = gamesense_color

client.set_event_callback('item_pickup', function(e)
    if e.item == 'defuser' then
        defusers = defusers + 1
    end
end)

client.set_event_callback('item_remove', function(e)
    if e.item == 'defuser' and defusers >= 1 then
        defusers = defusers - 1
    end
end)

client.set_event_callback('bomb_beginplant',function(e)
	planting_started_at = globals.curtime()
end)

local function reset()
	planting_started_at = nil
end
client.set_event_callback('round_start', reset)
client.set_event_callback('bomb_abortplant', reset)
client.set_event_callback('bomb_planted', reset)

client.set_event_callback('paint', function(ctx)
	if planting_started_at ~= nil then
		local game_rules_proxy = entity.get_game_rules()
		if entity.get_prop(game_rules_proxy, 'm_bBombPlanted') == 1 then
			return
		end

		local finished_at = (planting_started_at + planting_time)
		local round_end_time = entity.get_prop(game_rules_proxy, 'm_fRoundStartTime') + entity.get_prop(game_rules_proxy, 'm_iRoundTime')
		local has_time = round_end_time > finished_at

		planting_color = has_time and gamesense_color or {255, 0, 0, 255}

		local plant_percentage = (globals.curtime() - planting_started_at) / planting_time
		local text_x, text_y = surface.get_text_size(bomb_font, 'A - Planting')
		
		draw_indicator_circle(ctx, text_x + 23, text_y - 3, 255, 255, 255, 255, 9, plant_percentage)
	else
		local c4_entities = entity.get_all('CPlantedC4')
		for i = 1, #c4_entities do
			local c4 = c4_entities[i]
			
			if entity.get_prop(c4, 'm_bBombDefused') ~= 1 then
				local bomb_time = entity.get_prop(c4, 'm_flC4Blow') - globals.curtime()
				bomb_color = ((bomb_time < 5 and defusers >= 1) or (bomb_time < 10 and defusers == 0)) and {255, 0, 0, 255} or gamesense_color
			end

			if entity.get_prop(c4, 'm_bBombTicking') == 1 then
				local player = entity.get_prop(c4, 'm_hBombDefuser')
				if player ~= nil then
					local defused_at = entity.get_prop(c4, 'm_flDefuseCountDown')
					local defuse_length = entity.get_prop(c4, 'm_flDefuseLength')

					if defused_at == nil or defuse_length == nil then return end

					local defuse_percentage = globals.curtime() - (defused_at - defuse_length)
					local explodes_at = entity.get_prop(c4, 'm_flC4Blow')

					if defuse_length~=0 and defuse_percentage > 0 then
						local defuse_time = defuse_length - defuse_percentage
						local defuse_color = defuse_time > explodes_at - globals.curtime() and {255, 0, 0, 255} or {30, 144, 255, 255}
						renderer.indicator(defuse_color[1], defuse_color[2], defuse_color[3], defuse_color[4], 'Defuse - ', string.format('%.1f', defuse_time), 's')
					end
					return
				end
			end
		end
	end
end)

local indicators = {}
local top_indicators = {}
local bottom_indicators = {}

local blacklist = {
    'Bombsite ',
    'A - ',
    'B - ',
    ' HP',
	'FATAL',
	'Defuse - '
}

local function indicators_override(indicator)
    for _, blacklisted_text in f.pairs(blacklist) do
		if string.match(indicator.text, blacklisted_text) then
			if string.match(indicator.text, blacklist[2]) or string.match(indicator.text, blacklist[3]) then
				indicator.r, indicator.g, indicator.b, indicator.a = table.unpack(bomb_color)
			end
			if string.match(indicator.text, blacklist[1]) then
				indicator.text = string.gsub(indicator.text,'Bombsite ', '')..' - Planting'
				indicator.r, indicator.g, indicator.b, indicator.a = table.unpack(planting_color)
			end
			table.insert(top_indicators, indicator)
            return
        end
	end
	
	if indicator.text == 'DT' then
		if indicator.g ~= 0 then
			indicator.r, indicator.g, indicator.b, indicator.a = table.unpack(gamesense_color)
		end
	end

	bottom_indicators[indicator.text] = {indicator.r, indicator.g, indicator.b, indicator.a}
end

local screen = {} screen.w, screen.h = client.screen_size()
local height = screen.h - 70

client.set_event_callback('paint', function(ctx)
	indicators = {}

	local m_indicators = {
		{
			text = f.tostring(choked_ticks_max),
			color = {60, 120, 180, 255},
			bool = contains(menu.get, menu.fl)
		},
		{
			text = f.tostring(ui.get(refs.dmg)),
			color = get_custom_reference('mindmg', {'Rage', 'Other', 'Override damage'}) or {186, 174, 214, 255},
			bool = get_custom_reference(mindmg.name, mindmg.ref)
		},
		{
			text = 'DUCK',
			color = bottom_indicators['DUCK'],
			bool = bottom_indicators['DUCK']
		},
		{
			text = 'PING',
			color = bottom_indicators['PING'],
			bool = bottom_indicators['PING']
		},
		{
			text = 'BAIM',
			color = {255, 0, 0, 255},
			bool = ui.get(refs.baim)
		},
		{
			text = 'FS',
			color = gamesense_color,
			bool = #ui.get(refs.freestand[1]) ~= 0 and ui.get(refs.freestand[2])
		},
		{
			text = 'SP',
			color = {30, 144, 255, 255},
			bool = ui.get(refs.safepoint)
		},
		{
			text = 'LC',
			color = bottom_indicators['LC'],
			bool = bottom_indicators['LC']
		},
		{
			text = 'FAKE',
			color = {get_color_by_float(math.max(0, math.min(1, desync_delta / 58)), 255, 0, 0, 255, 255, 255, 0, 255, 123, 194, 21, 255)},
			bool = ui.get(refs.aa)
		},
		{
			text = 'DT',
			color = bottom_indicators['DT'],
			bool = bottom_indicators['DT']
		},
		{
			text = 'HIDE',
			color = gamesense_color,
			bool = not bottom_indicators['DT'] and ui.get(refs.on_shot[1]) and ui.get(refs.on_shot[2])
		}
		
	}
	
local r,g,b,a = ui.get(clrpick) 
local r1,g1,b1,a1 = ui.get(clrpick1)

	

	local spacing = 30

	if contains(menu.get, menu.ind) or contains(menu.get, menu.n_ind) then
		client.set_event_callback('indicator',indicators_override)

		for index, value in f.pairs(m_indicators) do
			if value.bool then
				indicators[#indicators+1] = {r = value.color[1], g = value.color[2], b = value.color[3], a = value.color[4], text = value.text}
			end
		end

		for index, indicator in f.pairs(top_indicators) do
			index = index - 1
			local offset = spacing * index
			surface.draw_text(10 + 1, 10 + offset + 1, 0, 0, 0, 255, bomb_font, indicator.text)
			surface.draw_text(10, 10 + offset, indicator.r, indicator.g, indicator.b, indicator.a, bomb_font, indicator.text)
		end

		top_indicators = {}

		if not entity.is_alive(entity.get_local_player()) then return end

		for index, indicator in f.pairs(indicators) do
			index = index - 1
			local offset = spacing * index

			if contains(menu.get, menu.ind) then
				surface.draw_text(10 + 1, height - offset + 1, 0, 0, 0, 255, font, indicator.text)
				surface.draw_text(10, height - offset, indicator.r, indicator.g, indicator.b, indicator.a, font, indicator.text)
			elseif contains(menu.get, menu.n_ind) then
				renderer.text(10, height - offset, indicator.r, indicator.g, indicator.b, indicator.a, "d+", 0, indicator.text)
			end
		end

		if contains(menu.get, menu.fl) then
			local x, y = surface.get_text_size(font, f.tostring(choked_ticks_max))
			local radius = 8.2
			draw_indicator_circle(ctx, x + 20, height + 14, 60, 120, 180, 255 / 4, radius, choked_ticks_max/fakelag_maximum)
			draw_indicator_circle(ctx, x + 20, height + 14, 60, 120, 180, 255, radius, choked_ticks/fakelag_maximum)
		end

		bottom_indicators = {}
	else
		client.unset_event_callback('indicator', indicators_override)
	end
end)






client.set_event_callback('shutdown', function()
	client.unset_event_callback('indicator',indicators_override)
	ui.set(refs.sv_maxusrcmdprocessticks, 16)
end)
