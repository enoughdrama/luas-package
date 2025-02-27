local globals_frametime = globals.frametime 
local globals_tickinterval = globals.tickinterval
local entity_is_enemy = entity.is_enemy
local entity_get_prop = entity.get_prop
local surface = require 'gamesense/surface'
local csgo_weapons = require 'gamesense/csgo_weapons'
local vector = require 'vector'
local bit = require 'bit'
local entity_is_dormant = entity.is_dormant
local entity_is_alive = entity.is_alive
local entity_get_origin = entity.get_origin
local entity_get_local_player = entity.get_local_player
local entity_get_player_resource = entity.get_player_resource
local table_insert = table.insert
local math_floor = math.floor
local player_memory = {}
local saf = 0
local enabled_ref = ui.new_checkbox('RAGE', 'Aimbot', 'Log aimbot shots')
local logautodir = ui.new_checkbox('lua', 'b', 'Log AutoPosition')
local menu = {
    x_offset = ui.new_slider('lua','b','Log x offset', 0, 2560, 14, true, "px", 1),
    y_offset = ui.new_slider('lua','b','Log y offset', 0, 1440, 14, true, "px", 1)
}
local function autodir()
    if ui.get(logautodir) then
    ui.set(menu.x_offset, 20)
    ui.set(menu.y_offset, 80)
end
end

local function autodir1()
    if ui.get(logautodir) then
    ui.set(menu.x_offset, 13)
    ui.set(menu.y_offset, 4)
end
end

local client_exec, client_log, client_set_event_callback, client_userid_to_entindex, entity_get_local_player, plist_get = client.exec, client.log, client.set_event_callback, client.userid_to_entindex, entity.get_local_player, plist.get


local function on_paint()
    local health = entity.get_prop(entity.get_local_player(), 'm_iHealth')

    if health == 0 then 
        autodir1()
    elseif health > 0 then
        autodir()
    end
end
client.set_event_callback("paint", on_paint)


local function on_ui()
    if not ui.get(logautodir) then
    ui.set_visible(menu.x_offset, true)
    ui.set_visible(menu.y_offset, true)
    else
    ui.set_visible(menu.x_offset, false)
    ui.set_visible(menu.y_offset, false)
    end
end
    on_ui()
    ui.set_callback(logautodir, on_ui)

    local dt_ch = ui.reference('rage', 'other', 'double tap')
    local omgjs = 0
    local shots = 0
    local android_notify=(function()local a={callback_registered=false,maximum_count=7,data={}}function a:register_callback()if self.callback_registered then return end;client.set_event_callback('paint_ui',function()local b={client.screen_size()}local c={56,56,57}local d=5;local e=self.data;for f=#e,1,-1 do self.data[f].time=self.data[f].time-globals.frametime()local g,h=255,0;local i=e[f]if i.time<0 then table.remove(self.data,f)else local j=i.def_time-i.time;local j=j>1 and 1 or j;if i.time<0.5 or j<0.5 then h=(j<1 and j or i.time)/0.5;g=h*255;if h<0.2 then d=d+15*(1.0-h/0.2)end end;local k={renderer.measure_text(nil,i.draw)}local l={b[1]/2-k[1]/2+3,b[2]-b[2]/100*17.4+d}renderer.circle(l[1],l[2],c[1],c[2],c[3],g,20,180,0.5)renderer.circle(l[1]+k[1],l[2],c[1],c[2],c[3],g,20,0,0.5)renderer.rectangle(l[1],l[2]-20,k[1],40,c[1],c[2],c[3],g)renderer.text(l[1]+k[1]/2,l[2],255,255,255,g,'c',nil,i.draw)d=d-50 end end;self.callback_registered=true end)end;function a:paint(m,n)local o=tonumber(m)+1;for f=self.maximum_count,2,-1 do self.data[f]=self.data[f-1]end;self.data[1]={time=o,def_time=o,draw=n}self:register_callback()end;return a end)()
    local ffi = require 'ffi'
    cvar.cl_showerror:set_int(0)
    local surface_measure_text, surface_draw_text = surface.measure_text, surface.draw_text
    local verdana = surface.create_font('Verdana', 12, 500, { 0x500 --[[ Outline ]] })
    
    local weakratshox =
        (function()
        local a = {callback_registered = false, maximum_count = 6, data = {}}
        function a:register_callback()
            if self.callback_registered then
                return
            end
            client.set_event_callback(
                "paint_ui",
                function()
                    local c = {56, 56, 57}
                    local d = ui.get(menu.y_offset)
                    local e = self.data
                    for f = #e, 1, -1 do
                        self.data[f].time = self.data[f].time - globals.frametime()
                        local g, h = 255, 0
                        local i = e[f]
                        if i.time < 0 then
                            table.remove(self.data, f)
                        else
                            local j = i.def_time - i.time
                            local j = j > 1 and 1 or j
                            if i.time < 0.5 or j < 0.5 then
                                h = (j < 1 and j or i.time) / 0.5
                                g = h * 255
                                if h < 0.2 then
                                    d = d - 15 * (1.0 - h / 0.2)
                                end
                            end
                            surface.draw_text(ui.get(menu.x_offset), ui.get(menu.y_offset) + d + 1,0,0,0,g, verdana, i.draw)
                            surface.draw_text(ui.get(menu.x_offset), ui.get(menu.y_offset) + d,255,255,255,g, verdana, i.draw)
                            d = d + 15
                        end
                    end
                    self.callback_registered = true
                end
            )
        end
        function a:paint(m, n)
            local o = tonumber(m) + 1
            for f = self.maximum_count, 2, -1 do
                self.data[f] = self.data[f - 1]
            end
            self.data[1] = {time = o, def_time = o, draw = n}
            self:register_callback()
        end
        return a
    end)()
    local shoxrat = function ( txt )
        weakratshox:paint(8, txt)
    end
    local tickrate = 1/globals.tickinterval()
    local client_color_log = client.color_log

local function Clearshots1(Event)
    if shots == 15 then shots = 0 end
end
client.set_event_callback('run_command', Clearshots1)

local function time_to_ticks(t)
	return math.floor(0.5 + (t / globals.tickinterval()))
end

local function tickrate_upd()
	tickrate = 1/globals.tickinterval()
end

local function compare(tab, val)
    for i = 1, #tab do
        if tab[i] == val then
            return true
        end
    end
    
    return false
end

local console_log = client.log
local get_local_player = entity.get_local_player
local get_all = entity.get_all
local get_prop = entity.get_prop
local get_name = entity.get_player_name
local string_format = string.formrue
local get_all_players = entity.get_players
local console_cmd = client.exec
local ui_get = ui.get
local global_realtime = globals.realtime
local draw_text = client.draw_text
local get_screen_size = client.screen_size
local sw, sh = get_screen_size()
local x, y = sw-20, sh-30

 
local function contains(tab, val)
    for key, value in   pairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end


local vector = require 'vector'

local master_switch = ui.reference('RAGE', 'Aimbot', 'Enabled')
local force_safe_point = ui.reference('RAGE', 'Aimbot', 'Force safe point')

local time_to_ticks = function(t) return math_floor(0.5 + (t / globals_tickinterval())) end
local vec_substract = function(a, b) return { a[1] - b[1], a[2] - b[2], a[3] - b[3] } end
local vec_lenght = function(x, y) return (x * x + y * y) end

local g_impact = { }
local g_aimbot_data = { }
local g_sim_ticks, g_net_data = { }, { }

local cl_data = {
    tick_shifted = false,
    tick_base = 0
}

local float_to_int = function(n) 
	return n >= 0 and math.floor(n+.5) or math.ceil(n-.5)
end

local get_entities = function(enemy_only, alive_only)
    local enemy_only = enemy_only ~= nil and enemy_only or false
    local alive_only = alive_only ~= nil and alive_only or true
    
    local result = {}
    local player_resource = entity_get_player_resource()
    
    for player = 1, globals.maxplayers() do
        local is_enemy, is_alive = true, true
        
        if enemy_only and not entity_is_enemy(player) then is_enemy = false end
        if is_enemy then
            if alive_only and entity_get_prop(player_resource, 'm_bAlive', player) ~= 1 then is_alive = false end
            if is_alive then table_insert(result, player) end
        end
    end

    return result
end

local generate_flags = function(e, on_fire_data)
    return {
		e.refined and 'R' or '',
		e.expired and 'X' or '',
		e.noaccept and 'NOA' or '',
		cl_data.tick_shifted and 'S' or '',
		on_fire_data.teleported and 'T' or '',
		on_fire_data.interpolated and 'I' or '',
		on_fire_data.extrapolated and 'EX' or '',
		on_fire_data.boosted and 'B' or '',
		on_fire_data.high_priority and 'H' or ''
    }
end



local hitgroup_names = { 'generic', 'head', 'chest', 'stomach', 'left arm', 'right arm', 'left leg', 'right leg', 'neck', '?', 'gear' }
local weapon_to_verb = { knife = 'Knifed', hegrenade = 'Naded', inferno = 'Burned', taser = 'Zeused'}

--region net_update
local function g_net_update()
	local me = entity_get_local_player()
    local players = get_entities(true, true)
	local m_tick_base = entity_get_prop(me, 'm_nTickBase')
	
    cl_data.tick_shifted = false
    
	if m_tick_base ~= nil then
		if cl_data.tick_base ~= 0 and m_tick_base < cl_data.tick_base then
			cl_data.tick_shifted = true
		end
	
		cl_data.tick_base = m_tick_base
    end

	for i=1, #players do
		local idx = players[i]
        local prev_tick = g_sim_ticks[idx]
        
        if entity_is_dormant(idx) or not entity_is_alive(idx) then
            g_sim_ticks[idx] = nil
            g_net_data[idx] = nil
        else
            local player_origin = { entity_get_origin(idx) }
            local simulation_time = time_to_ticks(entity_get_prop(idx, 'm_flSimulationTime'))
    
            if prev_tick ~= nil then
                local delta = simulation_time - prev_tick.tick

                if delta < 0 or delta > 0 and delta <= 64 then
                    local m_fFlags = entity_get_prop(idx, 'm_fFlags')

                    local diff_origin = vec_substract(player_origin, prev_tick.origin)
                    local teleport_distance = vec_lenght(diff_origin[1], diff_origin[2])

                    g_net_data[idx] = {
                        tick = delta-1,

                        origin = player_origin,
                        tickbase = delta < 0,
                        lagcomp = teleport_distance > 4096,
                    }
                end
            end

            g_sim_ticks[idx] = {
                tick = simulation_time,
                origin = player_origin,
            }
        end
    end
end
--endregion


local function g_aim_fire(e)
    local data = e
    local plist_sp = plist.get(e.target, 'Override safe point')
    local plist_fa = plist.get(e.target, 'Correction active')
    local checkbox = ui.get(force_safe_point)

    if g_net_data[e.target] == nil then
        g_net_data[e.target] = { }
    end

    data.tick = e.tick
    data.backtrack = e.backtrack
    data.eye = vector(client.eye_position)
    data.shot = vector(e.x, e.y, e.z)

    data.teleported = g_net_data[e.target].lagcomp or false
    data.choke = g_net_data[e.target].tick or '?'
    data.self_choke = globals.chokedcommands()
    data.correction = plist_fa and 1 or 0
    data.safe_point = ({
        ['Off'] = 'off',
        ['On'] = true,
        ['-'] = checkbox
    })[plist_sp]
    g_aimbot_data[e.id] = data
end

local function g_aim_hit(e)
    shots = shots + 1
    if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
        return
    end
    local on_fire_data = g_aimbot_data[e.id]
	local name = string.lower(entity.get_player_name(e.target))
	local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
    local aimed_hgroup = hitgroup_names[on_fire_data.hitgroup + 1] or '?'
    
    local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'
    local health = entity_get_prop(e.target, 'm_iHealth')

    local flags = generate_flags(e, on_fire_data)

    if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
        return
    end

    local on_fire_data = g_aimbot_data[e.id]
    local name = string.lower(entity.get_player_name(e.target))

	local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
    local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'

    local flags = generate_flags(e, on_fire_data)

    local inaccuracy = 0
    for i=#g_impact, 1, -1 do
        local impact = g_impact[i]

        if impact and impact.tick == globals.tickcount() then
            local aim, shot = 
                (impact.origin-on_fire_data.shot):angles(),
                (impact.origin-impact.shot):angles()

            inaccuracy = vector(aim-shot):length2d()
        end
    end

local group = hitgroup_names[e.hitgroup + 1] or "?"
local derg = math.random(0,58)

local safover = ui.reference('RAGE', 'Aimbot', 'Prefer safe point')
local safforce = ui.reference('RAGE', 'Aimbot' , 'Force safe point')

if ui.get(safover) then
    saf = 1
end
if ui.get(safforce) then
    saf = 1
end
if (ui.get(safover) ~= true) and (ui.get(safforce) ~= true) then
    saf = -1
end
local getbody = plist.get(e.target, "Override prefer body aim") == "Force"


cvar.cl_showerror:set_int(0)

      local xshot = "st"
      if shots == 1 then
      xshot = "st"
      end
      if shots == 2 then
      xshot = "nd"
      end
      if shots == 3 then 
      xshot = "rd"
      end
      if shots > 3 then 
        xshot = "th"
      end
local dtkey, dt_key = ui.reference('rage', 'other', 'double tap')
local hsbind, hskey = ui.reference('aa', 'other', 'on shot anti-aim')
local flagss = math.random(0,5)
    local flagss = math.random(0,3)
    if flagss == 0 then
        generate_flags1 = '101'
    elseif flagss == 1 then
        generate_flags1 = '000'
    elseif flagss == 2 then
        generate_flags1 = '001'
    elseif flagss == 3 then
        generate_flags1 = '100'
    end
local oops = ''
local ping = on_fire_data.backtrack
end



local function g_aim_miss(e)
    shots = shots + 1
    if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
        return
    end
    local xyita = ""
    local on_fire_data = g_aimbot_data[e.id]
    local name = string.lower(entity.get_player_name(e.target))

	local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
    local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'

    local flags = generate_flags(e, on_fire_data)

   
    local inaccuracy = 0
    for i=#g_impact, 1, -1 do
        local impact = g_impact[i]

        if impact and impact.tick == globals.tickcount() then
            local aim, shot = 
                (impact.origin-on_fire_data.shot):angles(),
                (impact.origin-impact.shot):angles()

            inaccuracy = vector(aim-shot):length2d()
            break
        end
    end
local derg = math.random(0,58)
local safover = ui.reference('RAGE', 'Aimbot', 'Prefer safe point')
local safforce = ui.reference('RAGE', 'Aimbot' , 'Force safe point')

if ui.get(safover) then
    saf = 1
end
if ui.get(safforce) then
    saf = 1
end
if (ui.get(safover) ~= true) and (ui.get(safforce) ~= true) then
    saf = -1
end
local ping = on_fire_data.backtrack
local dtkey, dt_key = ui.reference('rage', 'other', 'double tap')
local hsbind, hskey = ui.reference('aa', 'other', 'on shot anti-aim')
local flagss = math.random(0,5)
    local flagss = math.random(0,3)
    if flagss == 0 then
        generate_flags1 = '101'
    elseif flagss == 1 then
        generate_flags1 = '000'
    elseif flagss == 2 then
        generate_flags1 = '001'
    elseif flagss == 3 then
        generate_flags1 = '100'
    end

local oops = ''


    if shots == 1 then
        xshot = "st"
        end
        if shots == 2 then
        xshot = "nd"
        end
        if shots == 3 then 
        xshot = "rd"
        end
        if shots > 3 then 
          xshot = "th"
        end

        local ping = on_fire_data.backtrack


end
local shoot_time = { }


local handle_aimbot = function(e)
    if e.reason == nil then
        shoot_time[e.target] = globals.curtime()
        return
    end
    shots = shots + 1
    if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
        return
    end

    local on_fire_data = g_aimbot_data[e.id]
    local name = string.lower(entity.get_player_name(e.target))

	local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
    local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'

    local flags = generate_flags(e, on_fire_data)
    local inaccuracy = 0
    for i=#g_impact, 1, -1 do
        local impact = g_impact[i]

        if impact and impact.tick == globals.tickcount() then
            local aim, shot = 
                (impact.origin-on_fire_data.shot):angles(),
                (impact.origin-impact.shot):angles()

            inaccuracy = vector(aim-shot):length2d()
            break
        end
    end
local derg = math.random(0,58)
local safover = ui.reference('RAGE', 'Aimbot', 'Prefer safe point')
local safforce = ui.reference('RAGE', 'Aimbot' , 'Force safe point')

if ui.get(safover) then
    saf = 1
end
if ui.get(safforce) then
    saf = 1
end
if (ui.get(safover) ~= true) and (ui.get(safforce) ~= true) then
    saf = -1
end
local ping = on_fire_data.backtrack
local dtkey, dt_key = ui.reference('rage', 'other', 'double tap')
local hsbind, hskey = ui.reference('aa', 'other', 'on shot anti-aim')
local flagss = math.random(0,5)
    local flagss = math.random(0,3)
    if flagss == 0 then
        generate_flags1 = '101'
    elseif flagss == 1 then
        generate_flags1 = '000'
    elseif flagss == 2 then
        generate_flags1 = '001'
    elseif flagss == 3 then
        generate_flags1 = '100'
    end

local oops = ''



    if shots == 1 then
        xshot = "st"
        end
        if shots == 2 then
        xshot = "nd"
        end
        if shots == 3 then 
        xshot = "rd"
        end
        if shots > 3 then 
          xshot = "th"
        end
        local droppeed = math.random(0,1)
        local ping = on_fire_data.backtrack

        if e.reason == 'prediction error' then
            generate_flags1 = '011'
        end
        local velmod = math.random(1,9)
    local delay = ((globals.curtime()-shoot_time[e.target])-client.latency())*1000

end

client.set_event_callback('aim_fire', handle_aimbot)
client.set_event_callback('aim_miss', handle_aimbot)
local function shutdown()
    client.exec('clear')
    cvar.cl_showerror:set_int(0)
    cvar.cl_drawhud_force_radar:set_int(1)
    autodir()
end
client.set_event_callback('shutdown', shutdown)

local function g_player_hurt(e)
    local attacker_id = client.userid_to_entindex(e.attacker)
	
    if not ui.get(master_switch) or attacker_id == nil or attacker_id ~= entity.get_local_player() then
        return
    end

    local group = hitgroup_names[e.hitgroup + 1] or "?"
	
    if group == 'generic'and weapon_to_verb[e.weapon] ~= nil then
        local target_id = client.userid_to_entindex(e.userid)
		local target_name = entity.get_player_name(target_id)

    end


end

local purchase_log_ref = ui.reference("MISC", "Miscellaneous", "Log weapon purchases")
local damage_log_ref = ui.reference("MISC", "Miscellaneous", "Log damage dealt")
local spread_log_ref = ui.reference("RAGE", "Aimbot", "Log misses due to spread")
local force_safe_point = ui.reference('RAGE', 'Aimbot', 'Force safe point')


cvar.cl_drawhud_force_radar:set_int(-1)

local function dis()
    if (ui.get(enabled_ref) == true) then
    cvar.cl_showerror:set_int(0)
end
end
ui.set_callback(enabled_ref, dis)
local function g_bullet_impact(e)
    local tick = globals.tickcount()
    local me = entity.get_local_player()
    local user = client.userid_to_entindex(e.userid)
    
    if user ~= me then
        return
    end

    if #g_impact > 150 and g_impact[#g_impact].tick ~= tick then
        g_impact = {}
    end

    g_impact[#g_impact+1] = 
    {
        tick = tick,
        origin = vector(client.eye_position()), 
        shot = vector(e.x, e.y, e.z)
    }
end
local shot_id = { }
--//struct definition
ffi.cdef[[  
    struct cope_log 
    {
    float time;
    char xyita_log[3];
    char log_text[250];
    };
]]

--//because lua doesn't have one credits to thelinx off of some random lua fourm
function math.clamp(...)
    local s = {...}
    table.sort(s)
    return s[2] --fixed //thelinx
end

local start_time = globals.realtime()
local hitgroup_names = { "generic", "head", "chest", "stomach", "left arm", "right arm", "left leg", "right leg", "neck", "?", "gear" }
local logs = {}
local shots = 0
local flagstable = { 100, 001, 101, 000}
--//main functions
local function draw_log( pos, string, time )
    local anim_time = 20
    local alpha = 200
    -- Rendering string with position provided in menu
    --renderer.text(ui.get(menu.x_offset), ui.get(menu.y_offset) + pos,255,255,255,alpha,"", 0, string)
    surface.draw_text(ui.get(menu.x_offset), ui.get(menu.y_offset) + pos,255,255,255,alpha, verdana, string)
end


local function handle_logs( )
    local pos = 1
            -- If log is last and logs more than 8, shift last log
    if #logs > 5 then
        table.remove( logs, 1 )
    end
    for i=1, #logs do
        if globals.realtime( ) - logs[i].time > 6 then
            table.remove( logs, i )
            return
        end
        draw_log( pos, ffi.string(logs[i].log_text, ffi.sizeof(logs[i].log_text)), logs[i].time )
        pos = pos + 15
    end
end

client.set_event_callback('aim_hit', function(e)

end)
    
client.set_event_callback("paint_ui", function()
        handle_logs()
end)





    local function Clearshots(Event)
        if shots == 15 then shots = 0 end
    end



client.set_event_callback('run_command', Clearshots)
client.set_event_callback('aim_fire', g_aim_fire)
client.set_event_callback('aim_hit', g_aim_hit)
client.set_event_callback('aim_miss', g_aim_miss)
client.set_event_callback('net_update_end', g_net_update)
client.set_event_callback('player_hurt', g_player_hurt)


local table_insert = table.insert
local table_remove = table.remove
local table_concat = table.concat
local string_format = string.format
local math_floor = math.floor
local entity_get_all = entity.get_all
local entity_get_local_player = entity.get_local_player
local entity_get_player_resource = entity.get_player_resource
local client_screen_size = client.screen_size
local client_set_event_callback = client.set_event_callback
local client_unset_event_callback = client.unset_event_callback
local ui_get = ui.get
local entity_get_players = entity.get_players
local globals_realtime = globals.realtime
local globals_tickinterval = globals.tickinterval
local globals_frametime = globals.frametime
local globals_tickcount = globals.tickcount
local client_userid_to_entindex = client.userid_to_entindex
local ui_set_callback = ui.set_callback
local entity_is_alive = entity.is_alive
local entity_get_prop = entity.get_prop
local math_max, math_min = math.max, math.min
local ffi = type(jit) == "table" and jit.status() and require("ffi") or error("FFI isn't available for use! Allow unsafe scripts!")

local ffi_cast,ffi_typeof = ffi.cast, ffi.typeof
local function vmt_entry(instance, index, type)
    return ffi_cast(type, (ffi_cast("void***", instance)[0])[index])
end
local tTween = require("hitlog\\tween")
local tCron = require("hitlog\\cron")
vector = require("hitlog\\havoc_vector")
local cl_data = {
    tick_shifted = false,
    tick_base = 0
}

ffi.cdef[[
    typedef void*( __thiscall* get_client_entity_fn_87692764296 )( void*, int );
	struct CCSGOPlayerAnimstate_67813985419 {
		char pad[ 3 ];
		char m_bForceWeaponUpdate; //0x4
		char pad1[ 91 ];
		void* m_pBaseEntity; //0x60
		void* m_pActiveWeapon; //0x64
		void* m_pLastActiveWeapon; //0x68
		float m_flLastClientSideAnimationUpdateTime; //0x6C
		int m_iLastClientSideAnimationUpdateFramecount; //0x70
		float m_flAnimUpdateDelta; //0x74
		float m_flEyeYaw; //0x78
		float m_flPitch; //0x7C
		float m_flGoalFeetYaw; //0x80
		float m_flCurrentFeetYaw; //0x84
		float m_flCurrentTorsoYaw; //0x88
		float m_flUnknownVelocityLean; //0x8C
		float m_flLeanAmount; //0x90
		char pad2[ 4 ];
		float m_flFeetCycle; //0x98
		float m_flFeetYawRate; //0x9C
		char pad3[ 4 ];
		float m_fDuckAmount; //0xA4
		float m_fLandingDuckAdditiveSomething; //0xA8
		char pad4[ 4 ];
		float m_vOriginX; //0xB0
		float m_vOriginY; //0xB4
		float m_vOriginZ; //0xB8
		float m_vLastOriginX; //0xBC
		float m_vLastOriginY; //0xC0
		float m_vLastOriginZ; //0xC4
		float m_vVelocityX; //0xC8
		float m_vVelocityY; //0xCC
		char pad5[ 4 ];
		float m_flUnknownFloat1; //0xD4
		char pad6[ 8 ];
		float m_flUnknownFloat2; //0xE0
		float m_flUnknownFloat3; //0xE4
		float m_flUnknown; //0xE8
		float m_flSpeed2D; //0xEC
		float m_flUpVelocity; //0xF0
		float m_flSpeedNormalized; //0xF4
		float m_flFeetSpeedForwardsOrSideWays; //0xF8
		float m_flFeetSpeedUnknownForwardOrSideways; //0xFC
		float m_flTimeSinceStartedMoving; //0x100
		float m_flTimeSinceStoppedMoving; //0x104
		bool m_bOnGround; //0x108
		bool m_bInHitGroundAnimation; //0x109
		float m_flTimeSinceInAir; //0x10A
		float m_flLastOriginZ; //0x10E
		float m_flHeadHeightOrOffsetFromHittingGroundAnimation; //0x112
		float m_flStopToFullRunningFraction; //0x116
		char pad7[ 4 ]; //0x11A
		float m_flMagicFraction; //0x11E
		char pad8[ 60 ]; //0x122
		float m_flWorldForce; //0x15E
		char pad9[ 462 ]; //0x162
		float m_flMaxYaw; //0x334
	};
]]

local entity_list = ffi.cast( ffi.typeof( "void***" ), client.create_interface( "client.dll", "VClientEntityList003" ) )
local get_client_entity = ffi.cast( "get_client_entity_fn_87692764296", entity_list[ 0 ][ 3 ] )



local script = {
    hitgroup_names = {
    "generic",
    "head",
    "chest",
    "stomach",
    "left arm",
    "right arm",
    "left leg",
    "right leg",
    "neck",
    "?",
    "gear"
},
    tAimbotFlags = {
        sBoosted = "Boosted",
        sHighPriority = "High Priority",
        sInterpolated = "Interpolated",
        sExtrapolated = "Extrapolated",
        sTeleported = "Teleported",
        sSafePoint = "Safe Point",
        sCorrectionActive = "Correction Active",
        sExpired = "Expired",
        sNoaccept = "Noaccept",
        sRefined = "Refined",
        sShifted = "Shifted"
    },
    sAimbotHitFormat = "Hit %s in [Pred:%s Real:%s]. Damage [Pred:%s Real:%s Req:%s Remain:%s]. Hitchance [Real,Pred:%3.2f%% Req:%3.2f%%]. Flags [%s]. Choke [Client:%2d Target:%2d]. Body Yaw [Current:%.2f° Max:±%.2f°]. Spread [%s]. Backtrack [%s:%s]",
    sAimbotMissFormat = "Missed shot in %s of %s due to %s. Hitchance [Real,Pred:%3.2f%% Req.:%3.2f%%]. Flags [%s]. Choke [Client:%2d Target:%2d]. Body Yaw [Current:%.2f° Max:±%.2f°]. Spread [%s]. Backtrack [%s:%s]",
    tAimbotMissReason = {
        ["spread"] = "spread",
        ["prediction error"] = "prediction error",
        ["death"] = "death",
        ["unregistered shot"] = "prediction error [unregistered Shot]",
        ["?"] = "unknown",
        ["event timeout"] = "event timeout",
        ["damage rejected"] = "damage rejected",
        ["tickbase desynced"] = "tickbase desynced"
    },
    dmg = ui.reference("Rage", "Aimbot", "Minimum Damage"),
    hit = ui.reference("Rage", "Aimbot", "Minimum hit chance"),
    ForceSafePoint = ui.reference("RAGE", "Aimbot", "Force safe point"),
    tAimbotFireHistory = {},
    tAimbotResultHistory = {},
  tTweens = { },
  tTweens2 = { },
  tCrons = { },
  tCrons2 = { },
  
}

local script_fun = {
    GenerateAimbotFireHistoryFlagsTable = function(tAimbotFireHistoryData)
        local result = {}
        if tAimbotFireHistoryData.bBoosted then
            table_insert(result, script.tAimbotFlags.sBoosted)
        end
        if tAimbotFireHistoryData.bHighPriority then
            table_insert(result, script.tAimbotFlags.sHighPriority)
        end
        if tAimbotFireHistoryData.bInterpolated then
            table_insert(result, script.tAimbotFlags.sInterpolated)
        end
        if tAimbotFireHistoryData.bExtrapolated then
            table_insert(result, script.tAimbotFlags.sExtrapolated)
        end
        if tAimbotFireHistoryData.bTeleported then
            table_insert(result, script.tAimbotFlags.sTeleported)
        end
        if tAimbotFireHistoryData.bSafePoint then
            table_insert(result, script.tAimbotFlags.sSafePoint)
        end
        if tAimbotFireHistoryData.bCorrectionActive then
            table_insert(result, script.tAimbotFlags.sCorrectionActive)
        end
        if tAimbotFireHistoryData.bExpired then
            table_insert(result, script.tAimbotFlags.sExpired)
        end
        if tAimbotFireHistoryData.bNoaccept then
            table_insert(result, script.tAimbotFlags.sNoaccept)
        end
        if tAimbotFireHistoryData.bRefined then
            table_insert(result, script.tAimbotFlags.sRefined)
        end
        if tAimbotFireHistoryData.bShifted then
            table_insert(result, script.tAimbotFlags.sShifted)
        end
        return result
    end,
    GetAimbotResultDataTweenObject = function(nID)
        for _, _v in pairs(script.tAimbotResultHistory) do
          if _v.nID == nID then
            return _v
          end
        end
      end,
    GetAnimationState = function(_Entity)
        if not (_Entity) then
            return
        end
        local player_ptr = ffi.cast( "void***", get_client_entity(entity_list, _Entity))
        local animstate_ptr = ffi.cast( "char*" , player_ptr ) + 0x3914
        local state = ffi.cast( "struct CCSGOPlayerAnimstate_67813985419**", animstate_ptr )[0]
    
        
        return state
    end,
    GetPlayerMaxFeetYaw = function(self, _Entity)
        local S_animationState_t = self.GetAnimationState(_Entity)
        local nDuckAmount = S_animationState_t.m_fDuckAmount
        local nFeetSpeedForwardsOrSideWays = math_max(0, math_min(1, S_animationState_t.m_flFeetSpeedForwardsOrSideWays))
        local nFeetSpeedUnknownForwardOrSideways = math_max(1, S_animationState_t.m_flFeetSpeedUnknownForwardOrSideways)
        local nValue =
            (S_animationState_t.m_flStopToFullRunningFraction * -0.30000001 - 0.19999999) * nFeetSpeedForwardsOrSideWays +
            1
        if nDuckAmount > 0 then
            nValue = nValue + nDuckAmount * nFeetSpeedUnknownForwardOrSideways * (0.5 - nValue)
        end
        local nDeltaYaw = S_animationState_t.m_flMaxYaw * nValue
        return nDeltaYaw < 60 and nDeltaYaw >= 0 and nDeltaYaw or 0
    end
}

local tGameData = {
    bIsOnServer = false,
    _LocalPlayer = false,
    _PlayerResource = false,
    bTickShifted = false,
    nLocalPlayerTickBasePrev = 0,
    tPlayers = {},
    tPlayersDataNetUpdatePrev = {},
    tPlayersDataNetUpdate = {},
    tBulletImpacts = {},
    tBulletImpactsNextIndex = 0,
    tHurts = {},
    tHurtsNextIndex = 0
}
local tickrate = 1 / globals_tickinterval()
local function reset_aimdata()
    script.tAimbotFireHistory = {}
    script.tAimbotResultHistory = {}

    tGameData.bIsOnServer = false
    tGameData._LocalPlayer = false
    tGameData._PlayerResource = false
    tGameData.bTickShifted = false
    tGameData.nLocalPlayerTickBasePrev = 0
    tGameData.tPlayers = {}
    tGameData.tPlayersDataNetUpdatePrev = {}
    tGameData.tPlayersDataNetUpdate = {}
end
local function time_to_ticks(t)
    return math_floor(0.5 + (t / globals_tickinterval()))
end
local function tickrate_upd()
    tickrate = 1 / globals_tickinterval()
end
local function compare(tab, val)
    for i = 1, #tab do
        if tab[i] == val then
            return true
        end
    end
    return false
end

client_set_event_callback(
    "net_update_end",
    function(e)
        tGameData.bIsOnServer =
            tGameData._LocalPlayer and tGameData._PlayerResource and entity_is_alive(tGameData._LocalPlayer)
        if not (tGameData._LocalPlayer and tGameData._PlayerResource) then
            tGameData._LocalPlayer = entity_get_local_player()
            tGameData._PlayerResource = entity_get_player_resource()
        end
        if (not tGameData.bIsOnServer) then
            return
        end
        local nTickInterval = globals_tickinterval()
        do
            local _accum_0 = {}
            local _len_0 = 1
            local _list_0 = entity_get_players(false)
            for _index_0 = 1, #_list_0 do
                local nEntityIndex = _list_0[_index_0]

                _accum_0[_len_0] = nEntityIndex
                _len_0 = _len_0 + 1
            end
            tGameData.tPlayers = _accum_0
        end
        tGameData.bTickShifted = false

        local nLocalPlayerTickBase = entity_get_prop(tGameData._LocalPlayer, "m_nTickBase")
        cl_data.tick_shifted = false

        if nLocalPlayerTickBase ~= nil then
            if cl_data.tick_base ~= 0 and nLocalPlayerTickBase < cl_data.tick_base then
                cl_data.tick_shifted = true
            end

            cl_data.tick_base = nLocalPlayerTickBase

            local _list_0 = tGameData.tPlayers
            for _index_0 = 1, #_list_0 do
                local _Player = _list_0[_index_0]
                local nPlayerEntityIndex = _Player
                local tPlayersDataNetUpdatePrev = tGameData.tPlayersDataNetUpdatePrev[nPlayerEntityIndex]

                if entity_is_alive(nPlayerEntityIndex) then
                    local _PlayerWorldOrigin = vector(entity_get_prop(_Player, "m_vecOrigin"))

                    local nPlayerSimulationTime =
                        math_floor(0.5 + entity_get_prop(_Player, "m_flSimulationTime") / nTickInterval)
                    if tPlayersDataNetUpdatePrev then
                        local nPlayerSimulationTimeDelta =
                            nPlayerSimulationTime - tPlayersDataNetUpdatePrev.nPlayerSimulationTime
                        if
                            nPlayerSimulationTimeDelta < 0 or
                                nPlayerSimulationTimeDelta > 0 and nPlayerSimulationTimeDelta <= 1 / nTickInterval
                         then
                            tGameData.tPlayersDataNetUpdate[nPlayerEntityIndex] = {
                                nPlayerSimulationTimeDelta = nPlayerSimulationTimeDelta - 1,
                                _PlayerWorldOrigin = _PlayerWorldOrigin,
                                bTeleported = (vector(
                                    tPlayersDataNetUpdatePrev._PlayerWorldOrigin:distance(_PlayerWorldOrigin)
                                ):length_squared()) > 2048
                            }
                        end
                    end
                    tGameData.tPlayersDataNetUpdatePrev[nPlayerEntityIndex] = {
                        nPlayerSimulationTime = nPlayerSimulationTime,
                        _PlayerWorldOrigin = _PlayerWorldOrigin
                    }
                end
            end
        end
    end
)
client_set_event_callback(
    "aim_fire",
    function(e)
        --  e.id					Shot ID, this can be used to find the corresponding aim_hit / aim_miss event
        --  e.target				Target player entindex
        --  e.hit_chance			Chance the shot will hit, depends on spread
        --  e.hitgroup				Targeted hit group, this is not the same thing as a hitbox
        --  e.damage				Predicted damage the shot will do
        --  e.backtrack				Amount of ticks the player was backtracked
        --  e.boosted				True if accuracy boost was used to increase the accuracy of the shot
        --  e.high_priority			True if the shot was at a high priority record, like on shot backtrack
        --  e.interpolated			Player was interpolated
        --  e.extrapolated			Player was extrapolated
        --  e.teleported			Target player was teleporting (breaking lag compensation)
        --  e.tick					Tick the shot was fired at. This can be used to draw the hitboxes using client.draw_hitboxes
        --  e.x						X world coordinate of the aim point
        --  e.y						X world coordinate of the aim point
        --  e.z						Z world coordinate of the aim point

        local _TargetPlayer = e.target
        local sPlayerListOverrideSafePoint = plist.get(e.target, "Override safe point")
        local ex,ey,ez = e.x, e.y, e.z
        script.tAimbotFireHistory[e.id] = {
            nPredictedHitChance = e.hit_chance,
            sPredictedHitGroup = script.hitgroup_names[e.hitgroup + 1] or "?",
            nPredictedDamage = e.damage,
            nRequiredDamage = ui_get(script.dmg),
            nRequiredHitChance = ui_get(script.hit),
            bBoosted = e.boosted,
            bHighPriority = e.high_priority,
            bInterpolated = e.interpolated,
            bExtrapolated = e.extrapolated,
            bExpired = e.expired,
            bNoaccept = e.noaccept,
            bRefined = e.refined,
            backtrack = time_to_ticks(globals.curtime()),
            bShifted = cl_data.tick_shifted,
            nTick = e.tick,
            _WorldOrigin = vector(ex,ey,ez),
            nSelfChoke = e.self_choke or globals.chokedcommands(),
            nTargetChoke = tGameData.tPlayersDataNetUpdate[e.target].nPlayerSimulationTimeDelta or 0,
            bSafePoint = ({
                ["Off"] = false,
                ["On"] = true,
                ["-"] = ui_get(script.ForceSafePoint)
            })[sPlayerListOverrideSafePoint],
            bCorrectionActive = plist.get(e.target, "Correction active"),
            nBodyYaw = (entity_get_prop(_TargetPlayer, "m_flPoseParameter", 11) or 0) * 116 - 58,
            nMaxBodyYaw =   script_fun:GetPlayerMaxFeetYaw(_TargetPlayer),
            _LocalPlayerViewOrigin = entity_get_prop(entity_get_local_player(), "m_vecOrigin"),
            nLocalPlayerTotalHitsOnServer = entity_get_prop(entity_get_local_player(), "m_totalHitsOnServer"),
            nEventTimestamp = globals_realtime(),
            bTeleported =  e.teleported or tGameData.tPlayersDataNetUpdate[e.target].bTeleported or  false
        }
    end
)
client_set_event_callback(
    "aim_miss",
    function(e)
        -- e.id  			Shot ID, the corresponding aim_fire event has the same ID
        -- e.target 		Target player entindex
        -- e.hit_chance 	Actual hit chance the shot had
        -- e.hitgroup 		Hit group that was missed. This is not the same thing as a hitbox
        -- e.reason 		Reason the shot was missed. This can be 'spread', 'prediction error', 'death' or '?' (unknown / resolver)
        local _TargetPlayer = e.target
        local tAimbotFireHistoryData = script.tAimbotFireHistory[e.id]
        local tAimbotResultData = {
            tTweenData = {
              nProgress = 0,
              nBarProgress = 1
            }
          }
        tAimbotResultData.nEventTimestamp = globals_realtime()
        tAimbotResultData.nID = e.id
        tAimbotResultData.nTick = globals_tickcount()
        tAimbotResultData.sTargetPlayerName = entity.get_player_name(_TargetPlayer)
        tAimbotResultData.nActualHitChance = e.hit_chance
        tAimbotResultData.sActualHitGroup = script.hitgroup_names[e.hitgroup + 1] or "?"
        tAimbotResultData.nSpreadAngle = (function()
            local nAngle = 0
            local nFoundCount = 0
            local bOnce = false
            local _list_0 = tGameData.tBulletImpacts
            for _index_0 = 1, #_list_0 do
                local tBulletImpact = _list_0[_index_0]
                if tBulletImpact.nTick == tAimbotResultData.nTick and not bOnce then
                    nFoundCount = nFoundCount + 1
                   local v = (tAimbotFireHistoryData._WorldOrigin - tAimbotFireHistoryData._LocalPlayerViewOrigin)
                   local b = (tBulletImpact._WorldOrigin - tAimbotFireHistoryData._LocalPlayerViewOrigin)
                   nAngle = nAngle + (math.acos(v:dot_product(b) / (v:length() * b:length())))
                    bOnce = true
                end
            end
            return nFoundCount > 0 and math.deg(nAngle) or -1
        end)()
        if tAimbotFireHistoryData ~= nil and tAimbotFireHistoryData.backtrack ~=nil then
            tAimbotResultData.backtrack = time_to_ticks(globals.curtime()) - tAimbotFireHistoryData.backtrack
         else
        tAimbotResultData.backtrack = time_to_ticks(globals.curtime()) - 0
        end
        tAimbotResultData.backtrack_pos = tAimbotResultData.backtrack >= 0 and "Track" or "Pred"
        tAimbotResultData.backtrack_value = tAimbotResultData.backtrack >= 0 and tAimbotResultData.backtrack or tAimbotResultData.backtrack 

        tAimbotResultData.nLocalPlayerTotalHitsOnServer =
            entity_get_prop(entity_get_local_player(), "m_totalHitsOnServer")
        tAimbotResultData.sMissReason =
            (function()
            local sReason = script.tAimbotMissReason[e.reason]
            if sReason == "Spread" and tAimbotResultData.nSpreadAngle == -1 then
                return script.tAimbotMissReason["tickbase desynced"]
            end

            return script.tAimbotMissReason[e.reason] or "Unknown (" .. tostring(e.reason) .. ")"
        end)()
        tAimbotResultData.sFormattedData =
            string_format(
            script.sAimbotMissFormat,
            tAimbotResultData.sActualHitGroup,
            tAimbotResultData.sTargetPlayerName,
            tAimbotResultData.sMissReason,
            tAimbotResultData.nActualHitChance,
            tAimbotFireHistoryData.nRequiredHitChance,
            table_concat(script_fun.GenerateAimbotFireHistoryFlagsTable(tAimbotFireHistoryData), ", "),
            tAimbotFireHistoryData.nSelfChoke,
            tAimbotFireHistoryData.nTargetChoke,
            tAimbotFireHistoryData.nBodyYaw,
            tAimbotFireHistoryData.nMaxBodyYaw,
            tAimbotResultData.nSpreadAngle == -1 and "Unable to calculate:Tickbase Desynced" or
                string_format("%.2f°", tAimbotResultData.nSpreadAngle),
            tAimbotResultData.backtrack_pos,
            tAimbotResultData.backtrack_value
        )

        table_insert(script.tAimbotResultHistory, tAimbotResultData)
        local tTweenObject = script_fun.GetAimbotResultDataTweenObject(e.id)
        if tTweenObject then
          script.tTweens[e.id] = tTween.new(1, tTweenObject.tTweenData, {
            nProgress = 1
          }, "outQuint")
          script.tTweens2[e.id] = tTween.new(4, tTweenObject.tTweenData, {
            nBarProgress = 0
          })
          script.tCrons[e.id] = tCron.after(4, function()
            script.tTweens[e.id] = tTween.new(1, tTweenObject.tTweenData, {
              nProgress = 0
            }, "outQuint")
          end)
          script.tCrons2[e.id] = tCron.after(4 + 1, function()
            for _k, _v in pairs(script.tAimbotResultHistory) do
              if _v.nID == e.id then
                script.tAimbotResultHistory[_k] = nil
                script.tAimbotFireHistory[e.id] = nil
              end
            end
          end)
        end
        while #script.tAimbotResultHistory > 26 do
            local tAimbotResultHistoryData = table_remove(script.tAimbotResultHistory, 1)
            if tAimbotResultHistoryData and script.tAimbotFireHistory[tAimbotResultHistoryData.nID] then
                script.tAimbotFireHistory[tAimbotResultHistoryData.nID] = nil
            end
        end
        if e == nil then return end
        local ent = e.target
        local name = entity.get_player_name(ent)
        local hitnames = hitgroup_names[e.hitgroup + 1] or '?'
        local hc = math.floor(e.hit_chance)
        local degree = string.format("%.2f", client.random_float(-60.00, 60.00))
        local self_choke = globals.chokedcommands()
        local choke = name.tick or '?'
        local hitchance = math.floor(e.hit_chance)
        local safety = ({
            ['Off'] = 'off',
            ['On'] = true,
            ['-'] = checkbox
        })[plist_sp]

        local xyita = ""
        shots = shots + 1
        if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
            return
        end
        local xyita = ""
        local on_fire_data = g_aimbot_data[e.id]
        local name = string.lower(entity.get_player_name(e.target))
    
        local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
        local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'
    
        local flags = generate_flags(e, on_fire_data)
    
        local reason = script.tAimbotMissReason[e.reason]

      
        local inaccuracy = 0
        for i=#g_impact, 1, -1 do
            local impact = g_impact[i]
    
            if impact and impact.tick == globals.tickcount() then
                local aim, shot = 
                    (impact.origin-on_fire_data.shot):angles(),
                    (impact.origin-impact.shot):angles()
    
                inaccuracy = vector(aim-shot):length2d()
                break
            end
        end
    local derg = math.random(0,58)
    local safover = ui.reference('RAGE', 'Aimbot', 'Prefer safe point')
    local safforce = ui.reference('RAGE', 'Aimbot' , 'Force safe point')
    if ui.get(safover) then
        saf = 1
    end
    if ui.get(safforce) then
        saf = 1
    end
    if (ui.get(safover) ~= true) and (ui.get(safforce) ~= true) then
        saf = -1
    end
    local ping = on_fire_data.backtrack
    local dtkey, dt_key = ui.reference('rage', 'other', 'double tap')
    local hsbind, hskey = ui.reference('aa', 'other', 'on shot anti-aim')
    local flagss = math.random(0,5)
    local flagss = math.random(0,3)
    if flagss == 0 then
        generate_flags1 = '101'
    elseif flagss == 1 then
        generate_flags1 = '000'
    elseif flagss == 2 then
        generate_flags1 = '001'
    elseif flagss == 3 then
        generate_flags1 = '100'
    end
    
    local oops = ''
    
    
        if shots == 1 then
            xshot = "st"
            end
            if shots == 2 then
            xshot = "nd"
            end
            if shots == 3 then 
            xshot = "rd"
            end
            if shots > 3 then 
              xshot = "th"
            end
    
            local droppeed = math.random(0,1)
            local ping = on_fire_data.backtrack
            
            local del1 = math.random(1,100)
            local del2 = math.random(1,100)
            local velmod = math.random(1,20)
            local ebody = math.floor(tAimbotFireHistoryData.nBodyYaw)





            if ((ui.get(enabled_ref) == true) == true) and e.reason ~= 'death' and e.reason ~= 'prediciton error' and e.reason ~= 'unregistered shot' then 
                xyita = ''
                
            print(string.format(
                ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to %s [angle: %.2f° | 1:%i°] (dmg: %i | safety "..saf.." | history(Δ): %s | flags: %s )", 
                shots, name, hgroup, hitchance, reason, tAimbotResultData.nSpreadAngle, ebody,
                on_fire_data.damage, tAimbotResultData.backtrack_value, generate_flags1
            ))
            main_log = string.format(
                ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to %s [angle: %.2f° | 1: %i°] (dmg: %i | safety "..saf.." | history(Δ): %s | flags: %s )", 
                shots, name, hgroup, hitchance, reason, tAimbotResultData.nSpreadAngle, ebody,
                on_fire_data.damage, tAimbotResultData.backtrack_value, generate_flags1
            )

        elseif e.reason == 'death' and (ui.get(enabled_ref) == true) and e.reason ~= 'prediction error' and e.reason ~= 'unregistered shot' then
        print(string.format(
            ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to %s [angle: %.1f° | 1: %i°] (dropped: %i | flags: %s | error: 000 )", 
            shots, name, hgroup, hitchance, reason, tAimbotResultData.nSpreadAngle, ebody,droppeed,generate_flags1
            
        ))
        main_log = string.format(
            ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to %s [angle: %.1f° | 1: %i°] (dropped: %i | flags: %s | error: 000 )", 
            shots, name, hgroup, hitchance, reason, tAimbotResultData.nSpreadAngle, ebody,droppeed,generate_flags1
            
        )
    elseif e.reason == 'prediction error' and e.reason ~= 'death' and (ui.get(enabled_ref) == true) and e.reason ~= 'unregistered shot' then
        print(string.format(
            ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to %s [delay: %i:0.%i | dropped: %i] (vel_modifier: 0.%i | flags: %s | error: 000 )", 
            shots, name, hgroup, hitchance, reason,del1,del2,droppeed,velmod,generate_flags1
            
        ))
        main_log = string.format(
            ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to %s [delay: %i:0.%i | dropped: %i] (vel_modifier: 0.%i | flags: %s | error: 000 )", 
            shots, name, hgroup, hitchance, reason,del1,del2,droppeed,velmod,generate_flags1
            
        )
    elseif e.reason ~= 'prediction error' and e.reason ~= 'death' and (ui.get(enabled_ref) == true) and e.reason == 'unregistered shot' then
        print(string.format(
            ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to prediction error [unregistered shot] [delay: %i:0.%i | dropped: %i] (vel_modifier: 1.0| flags: %s | error: 110 )", 
            shots, name, hgroup, hitchance, del1,del2,droppeed,velmod,generate_flags1
            
        ))
        main_log =  string.format(
            ""..oops.."Missed %d"..xshot.." shot at %s\'s %s(%s) due to prediction error [unregistered shot] [delay: %i:0.%i | dropped: %i] (vel_modifier: 1.0| flags: %s | error: 110 )", 
            shots, name, hgroup, hitchance, del1,del2,droppeed,velmod,generate_flags1
            
        )
            end
            shoxrat(main_log)
        end
        )
    
client_set_event_callback("level_init", tickrate_upd)
client_set_event_callback("cs_game_disconnected", reset_aimdata)
client_set_event_callback("game_newmap", reset_aimdata)
--client_set_event_callback("round_end", reset_aimdata)
client_set_event_callback("player_connect_full", reset_aimdata)
client_set_event_callback(
    "aim_hit",
    function(e)
        --	e.id 				Shot ID, the corresponding aim_fire event has the same ID
        --	e.target			Target player entindex
        --	e.hit_chance		Actual hit chance the shot had
        --	e.hitgroup			Hit group that was hit. This is not the same thing as a hitbox
        --	e.damage			Actual damage the shot did

        local _TargetPlayer = e.target
        local tAimbotFireHistoryData = script.tAimbotFireHistory[e.id]
        local tAimbotResultData = {
            tTweenData = {
              nProgress = 0,
              nBarProgress = 1
            }
          }
        tAimbotResultData.nEventTimestamp = globals_realtime()
        tAimbotResultData.nID = e.id
        tAimbotResultData.health = entity.get_prop(_TargetPlayer, 'm_iHealth')
        tAimbotResultData.nTick = globals_tickcount()
        tAimbotResultData.sTargetPlayerName = entity.get_player_name(_TargetPlayer) or "Enemy"
        tAimbotResultData.nActualHitChance = e.hit_chance
        tAimbotResultData.sActualHitGroup = script.hitgroup_names[e.hitgroup + 1] or "?"
        tAimbotResultData.nActualDamage = e.damage
       tAimbotResultData.nSpreadAngle = (function()
            local nAngle = 0
            local nFoundCount = 0
            local bOnce = false
            local _list_0 = tGameData.tBulletImpacts
            for _index_0 = 1, #_list_0 do
                local tBulletImpact = _list_0[_index_0]
                if tBulletImpact.nTick == tAimbotResultData.nTick and not bOnce then
                    nFoundCount = nFoundCount + 1
                    local v = (tAimbotFireHistoryData._WorldOrigin - tAimbotFireHistoryData._LocalPlayerViewOrigin)
                    local b = (tBulletImpact._WorldOrigin - tAimbotFireHistoryData._LocalPlayerViewOrigin)
                    nAngle = nAngle + (math.acos(v:dot_product(b) / (v:length() * b:length())))
                    bOnce = true
                end
            end
            return nFoundCount > 0 and math.deg(nAngle) or -1
        end)()
        if tAimbotFireHistoryData ~=nil and tAimbotFireHistoryData.backtrack ~=nil then
        tAimbotResultData.backtrack = time_to_ticks(globals.curtime()) - tAimbotFireHistoryData.backtrack
        else
            tAimbotResultData.backtrack = time_to_ticks(globals.curtime()) - 0
        end
        tAimbotResultData.backtrack_pos = tAimbotResultData.backtrack >= 0 and "Track" or "Pred"
        tAimbotResultData.backtrack_value = tAimbotResultData.backtrack >= 0 and tAimbotResultData.backtrack or tAimbotResultData.backtrack 
        tAimbotResultData.sFormattedData =
            string_format(
            script.sAimbotHitFormat,
            tAimbotResultData.sTargetPlayerName,
            tAimbotFireHistoryData.sPredictedHitGroup,
            tAimbotResultData.sActualHitGroup,
            tAimbotFireHistoryData.nPredictedDamage,
            tAimbotResultData.nActualDamage,
            tAimbotFireHistoryData.nRequiredDamage,
            tAimbotResultData.health,
            tAimbotResultData.nActualHitChance,
            tAimbotFireHistoryData.nRequiredHitChance,
            table_concat(script_fun.GenerateAimbotFireHistoryFlagsTable(tAimbotFireHistoryData), ", "),
            tAimbotFireHistoryData.nSelfChoke,
            tAimbotFireHistoryData.nTargetChoke,
            tAimbotFireHistoryData.nBodyYaw,
            tAimbotFireHistoryData.nMaxBodyYaw,
            tAimbotResultData.nSpreadAngle == -1 and "Unable to calculate:Tickbase Desynced" or
                string.format("%.2f°", tAimbotResultData.nSpreadAngle),
            tAimbotResultData.backtrack_pos,
            tAimbotResultData.backtrack_value
        )

        table_insert(script.tAimbotResultHistory, tAimbotResultData)
        local tTweenObject = script_fun.GetAimbotResultDataTweenObject(e.id)
        if tTweenObject then
          script.tTweens[e.id] = tTween.new(1, tTweenObject.tTweenData, {
            nProgress = 1
          }, "outQuint")
          script.tTweens2[e.id] = tTween.new(4, tTweenObject.tTweenData, {
            nBarProgress = 0
          })
          script.tCrons[e.id] = tCron.after(4, function()
            script.tTweens[e.id] = tTween.new(1, tTweenObject.tTweenData, {
              nProgress = 0
            }, "outQuint")
          end)
          script.tCrons2[e.id] = tCron.after(4 + 1, function()
            for _k, _v in pairs(script.tAimbotResultHistory) do
              if _v.nID == e.id then
                script.tAimbotResultHistory[_k] = nil
                script.tAimbotFireHistory[e.id] = nil
              end
            end
          end)
        end
        while #script.tAimbotResultHistory > 26 do
            local tAimbotResultHistoryData = table_remove(script.tAimbotResultHistory, 1)
            if tAimbotResultHistoryData and script.tAimbotFireHistory[tAimbotResultHistoryData.nID] then
                script.tAimbotFireHistory[tAimbotResultHistoryData.nID] = nil
            end
        end
        shots = shots + 1
        if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
            return
        end
        local on_fire_data = g_aimbot_data[e.id]
        local name = string.lower(entity.get_player_name(e.target))
        local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
        local aimed_hgroup = hitgroup_names[on_fire_data.hitgroup + 1] or '?'
        
        local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'
        local health = entity_get_prop(e.target, 'm_iHealth')
    
        local flags = generate_flags(e, on_fire_data)
    
        if not ui.get(master_switch) or g_aimbot_data[e.id] == nil then
            return
        end
    
        local on_fire_data = g_aimbot_data[e.id]
        local name = string.lower(entity.get_player_name(e.target))
    
        local hgroup = hitgroup_names[e.hitgroup + 1] or '?'
        local hitchance = math_floor(on_fire_data.hit_chance + 0.5) .. '%'
    
        local flags = generate_flags(e, on_fire_data)
        local reason = script.tAimbotMissReason[e.reason]
    
        local inaccuracy = 0
        for i=#g_impact, 1, -1 do
            local impact = g_impact[i]
    
            if impact and impact.tick == globals.tickcount() then
                local aim, shot = 
                    (impact.origin-on_fire_data.shot):angles(),
                    (impact.origin-impact.shot):angles()
    
                inaccuracy = vector(aim-shot):length2d()
            end
        end
    
    local group = hitgroup_names[e.hitgroup + 1] or "?"
    local derg = math.random(0,58)
    
    local safover = ui.reference('RAGE', 'Aimbot', 'Prefer safe point')
    local safforce = ui.reference('RAGE', 'Aimbot' , 'Force safe point')
    if ui.get(safover) then
        saf = 1
    end
    if ui.get(safforce) then
        saf = 1
    end
    if (ui.get(safover) ~= true) and (ui.get(safforce) ~= true) then
        saf = -1
    end
    local getbody = plist.get(e.target, "Override prefer body aim") == "Force"
    
    
    cvar.cl_showerror:set_int(0)
    
          local xshot = "st"
          if shots == 1 then
          xshot = "st"
          end
          if shots == 2 then
          xshot = "nd"
          end
          if shots == 3 then 
          xshot = "rd"
          end
          if shots > 3 then 
            xshot = "th"
          end
    
        
    local dtkey, dt_key = ui.reference('rage', 'other', 'double tap')
    local hsbind, hskey = ui.reference('aa', 'other', 'on shot anti-aim')
    local flagss = math.random(0,5)
        local flagss = math.random(0,3)
        if flagss == 0 then
            generate_flags1 = '101'
        elseif flagss == 1 then
            generate_flags1 = '000'
        elseif flagss == 2 then
            generate_flags1 = '001'
        elseif flagss == 3 then
            generate_flags1 = '100'
        end
    
    local oops = ''
    
            local checkbox = ui.get(force_safe_point)
            local plist_sp = plist.get(e.target, 'Override safe point')
            local target = e.target
            local name = entity.get_player_name(target)
            local hitnames = hitgroup_names[e.hitgroup + 1] or '?'
            local degree = string.format("%.2f", client.random_float(-60.00, 60.00))
            local self_choke = globals.chokedcommands()
            local choke = name.tick or '?'
            local bober = "nothink"
            local id = e.id
            local hitchance = math.floor(e.hit_chance)
            local safety = ({
                ['Off'] = 'off',
                ['On'] = true,
                ['-'] = checkbox
            })[plist_sp]
            local xyita = ""
            local main_log = ""
    
    local ping = on_fire_data.backtrack
    local ebody = math.floor(tAimbotFireHistoryData.nBodyYaw)

        -- Как изотерик прописал
        if e.damage == on_fire_data.damage and hgroup == aimed_hgroup and group ~= 'generic' and (ui.get(enabled_ref) == true) then
            print(string.format(
               ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." | history(Δ): %s | flags: %s  )", 
               shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
               generate_flags1
           ))
           main_log = string.format(
            ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." | history(Δ): %s | flags: %s  )", 
            shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
            generate_flags1
           )
       end
       -- Нихуя не попало
       if e.damage ~= on_fire_data.damage and hgroup ~= aimed_hgroup and group ~= 'generic' and (ui.get(enabled_ref) == true) then
            print(string.format(
               ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." |  history(Δ): %s  | flags: %s | mismatch: [dmg: %d | hitgroup: %s] )", 
               shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
               generate_flags1, on_fire_data.damage, aimed_hgroup
           ))
           main_log = string.format(
            ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." |  history(Δ): %s  | flags: %s | mismatch: [dmg: %d | hitgroup: %s] )", 
            shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
            generate_flags1, on_fire_data.damage, aimed_hgroup
        )
       end
       
       -- Не в тот хитбокс
       if hgroup ~= aimed_hgroup and e.damage == on_fire_data.damage and group ~= 'generic' and (ui.get(enabled_ref) == true) then
            print(string.format(
               ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." | history(Δ): %s | flags: %s | mismatch: [hitgroup: %s] )", 
               shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
               generate_flags1, aimed_hgroup
           ))
           main_log = string.format(
            ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." | history(Δ): %s | flags: %s | mismatch: [hitgroup: %s] )", 
            shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
            generate_flags1, aimed_hgroup
        )
       end
       
       -- Дамаг не выбило
       
       if e.damage ~= on_fire_data.damage and hgroup == aimed_hgroup and group ~= 'generic' and (ui.get(enabled_ref) == true) then
            print(string.format(
               ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." | history(Δ): %s | flags: %s | mismatch: [dmg: %d] )",
               shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
               generate_flags1, on_fire_data.damage
           ))
           main_log = string.format(
            ""..oops.."Registered %d"..xshot.." shot in %s's %s for %i damage [angle: %.2f° | 1:%i°] (hitchance: %s | safety "..saf.." | history(Δ): %s | flags: %s | mismatch: [dmg: %d] )",
            shots, name, hgroup, e.damage, tAimbotResultData.nSpreadAngle, ebody,hitchance,tAimbotResultData.backtrack_value,
            generate_flags1, on_fire_data.damage
        )
       end
    
       shoxrat(main_log)
    end
    )

client_set_event_callback(
    "player_hurt",
    function(e)
        -- e.userid				user ID of who was hurt
        -- e.attacker			user ID of who attacked
        -- e.health				remaining health points
        -- e.armor				remaining armor points
        -- e.weapon				weapon name attacker used, if not the world
        -- e.dmg_health			damage done to health
        -- e.dmg_armor			damage done to armor
        -- e.hitgroup			hitgroup that was damaged

       

        local group = script.hitgroup_names[e.hitgroup + 1] or "?"
        local target_id = client_userid_to_entindex(e.userid)
        local team = entity.is_enemy(target_id) and "Enemy" or "Teammate"
        if group == "generic" then
            if e.weapon == nil or e.weapon == "" then
                local target_name = entity.get_player_name(target_id)
                print(
                    string_format(
                        "%s %s got Damage by World on [HP:%i AR:%i]. remaining [HP:%i AR:%i].",
                        team,
                        target_name,
                        e.dmg_health,
                        e.dmg_armor,
                        e.health,
                        e.armor
                    )
                )
            elseif e.weapon ~= nil and e.weapon ~= "taser" then
                local target_name = entity.get_player_name(target_id)
                local attacker_id = client_userid_to_entindex(e.attacker)
                if entity.is_enemy(attacker_id) then
                    return
                end
                if entity_get_local_player() == attacker_id then
                print(
                    string_format(
                        "%s got [HP:%i AR:%i] damage by %s, remaining [HP:%i  AR:%i]",
                        target_name,
                        e.dmg_health,
                        e.dmg_armor,
                        e.weapon,
                        e.health,
                        e.armor
                    )
                )
                end
            end
        end
    end
)

client_set_event_callback(
    "bullet_impact",
    function(e)
        -- e.userid				user ID of who was hurt
        -- e.x					X world coordinate of the bullet point
        -- e.y					Y world coordinate of the bullet point
        -- e.z					Z world coordinate of the bullet point
        local ex,ey,ez = e.x, e.y, e.z
        if entity_get_local_player() == client_userid_to_entindex(e.userid) then
            tGameData.tBulletImpactsNextIndex = tGameData.tBulletImpactsNextIndex + 1
            tGameData.tBulletImpacts[tGameData.tBulletImpactsNextIndex] = {
                nTick = globals_tickcount(),
                _WorldOrigin = vector(ex,ey,ez)
            }
            tGameData.tBulletImpactsNextIndex = tGameData.tBulletImpactsNextIndex % 26
        end
    end
)

