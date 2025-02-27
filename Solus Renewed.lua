
local clipboard = require 'neverlose/clipboard'
local base64 = require 'neverlose/base64'

local client_dll = ffi.cast('uint32_t**', utils.create_interface('client.dll', 'VClient018'))[0]
local client_mode = ffi.cast('void***', client_dll[10] + 0x5)[0][0]

local function get_type(value)
    if type(getmetatable(value)) == 'table' and value.__type then
        return value.__type.name:lower()
    end

    if type(value) == 'boolean' then
        value = value and 1 or 0
    end

    return type(value)
end

local oRenderText do
    oRenderText = render.text

    function render.text(id, pos, ...)
        oRenderText(id, (pos + .5):floor(), ...)
    end
end

local ctx = new_class()
    :struct 'menu' {
        xor_key = '84 9c 6a 00 e5 87 b1 4f 05 3c 64 0e c8 9a',

        initialize = function(self, ...)
            -- Apply Name / Icon
            ui.sidebar('\aA6C6F0FFSolus Renewed', 'paint-brush')

            -- Create Tabs
            -- GROUPS
            local general do
                general = { }

                local grp_main = ui.create('General', 'Features')
                local grp_res = ui.create('General', 'Resource Manager')
                local grp_presets = ui.create('General', 'Presets')

                grp_presets:button('Copy Preset', function() self:export() end)
                grp_presets:button('Paste Preset', function() self:import() end)

                general.watermark = grp_main:switch 'Watermark' do
                    local settings = general.watermark:create()

                    general.nickname = settings:combo('Nickname', { 'Default', 'Steam Name', 'Custom' })
                    general.custom_name = settings:input 'Custom Name'

                    general.nickname:set_callback(function(ref)
                        general.custom_name:set_visible(ref:get() == 'Custom')
                    end, true)

                    general.h12_format = settings:switch('12h Format', true)
                end

                general.exploiting = grp_main:switch 'Exploiting' do
                    local settings = general.exploiting:create()

                    general.exploiting_shifting_color = settings:color_picker('Shifting', color(228, 126, 10, 255))
                    general.exploiting_noentry_color = settings:color_picker('No Entry', color(235, 95, 135))
                    general.exploiting_lcu_color = settings:color_picker('LC Unsafe', color(219, 20, 60))
                    general.exploiting_lcb_color = settings:color_picker('LC Broken', color(64, 255, 194))
                end

                general.bomb = grp_main:switch 'Bomb' do
                    local settings = general.bomb:create()

                    general.bomb_dst_color = settings:color_picker('Distance Color', color(219, 20, 60, 200))
                    general.bomb_dmg_color = settings:color_picker('Damage Color', color(252, 243, 105))
                    general.bomb_dmg_fatal_color = settings:color_picker('Damage Fatal Color', color(255, 0, 50))
                end

                general.hotkeys = grp_main:switch 'Hotkeys' do
                    local settings = general.hotkeys:create()

                    general.hotkeys_sentence = settings:switch('Sentence Case', true)
                end

                general.spectators = grp_main:switch 'Spectators'

                general.muzzle = grp_main:switch 'Muzzle Attachment' do
                    local settings = general.muzzle:create()

                    general.muzzle_perspective = settings:selectable('Perspective', 'First Person', 'Third Person') : set('First Person', 'Third Person')
                    general.muzzle_smoothing = settings:slider('Smoothing', 50, 200, 100, .01)
                    general.muzzle_y_jiggle_factor = settings:slider('Y Jiggle Factor', 0, 100, 35)
                    general.muzzle_z_jiggle_factor = settings:slider('Z Jiggle Factor', 0, 100, 40)
                    general.muzzle_panel_switch_factor = settings:slider('Switch Factor', 0, 151, 151, 1, function(value) return value == 151 and 'Inf.' or value .. '%' end)
                    general.muzzle_panel_lean_offset = settings:slider('Lean Offset', 0, 256, 85, 1, function(value) return value .. 'px' end)
                    general.muzzle_panel_lean_angle = settings:slider('Lean Angle', 0, 360, 295, 1, function(value) return value .. '°' end)
                    general.muzzle_panel_3rd_person = settings:combo('3rd Person Pos', 'Auto', 'Left', 'Right')
                end

                general.muzzle_link = grp_main:switch '\aB6B665FFMuzzle Link' do
                    general.muzzle_link : set_tooltip 'This feature requires heavy render processing. Using it may lead to the vast amount of FPS drops.'

                    local settings = general.muzzle_link:create()

                    general.link_color = settings:color_picker('Color', color(34, 43, 113, 255))
                    general.link_step = settings:slider('Line Step', 2, 48, 6)
                    general.link_width = settings:slider('Line Width', 1, 16, 8)
                    general.link_interpolation = settings:slider('Interpolation', 2, 256, 32)
                    general.link_pulse_speed = settings:slider('Pulse', 0, 500, 0, .1, function(value)
                        if value == 0 then
                            return 'Off'
                        end
                    end)
                end

                self.general = general
                self.general.resources = { } do
                    local res = self.general.resources

                    res.text_color = grp_res:color_picker('Text Color', color(56, 121, 247))
                    res.background_color = grp_res:color_picker('Background Color', color(13, 13, 13, 125))

                    res.dpi_scale = grp_res:switch('Automatic DPI Scaling')

                    res.glow = grp_res:switch('Window Glow', true)
                    res.glow_color = res.glow:color_picker(color(23, 40, 125, 255))

                    res.outline = grp_res:switch('Window Outline', true) do
                        local settings = res.outline:create()

                        res.outline_color = settings:color_picker('Color \n outline:color', color(25, 25, 176))

                        res.outline_thickness = settings:slider('Thickness \n outline:thickness', 1, 8, 2)
                        res.outline_position = settings:combo('Position \n outline:position', { 'top', 'bottom', 'left', 'right' })
                    end

                    res.blur = grp_res:slider('Window Blur', 0, 100, 100, 1, function(value)
                        return value == 0 and 'Off' or value .. '%'
                    end)

                    res.rounding = grp_res:slider('Window Rounding', 0, 8, 4, 1, 'px')
                end
            end

            general.muzzle:set_callback(function(ref)
                general.muzzle_link:set_visible(ref:get())
            end, true)
        end,

        get_resources = function(self)
            local result = { }

            for name, value in pairs(self.general.resources) do
                result[name] = value:get()
            end

            if result.outline ~= true then
                result.outline_thickness = 0
            end

            if result.glow ~= true then
                result.glow_color = color(0, 0, 0, 0)
            end

            return result, section
        end,

        xorstr = function(self, str)
            local key = self.xor_key
            local strlen, keylen = #str, #key

            local strbuf = ffi.new('char[?]', strlen+1)
            local keybuf = ffi.new('char[?]', keylen+1)

            ffi.copy(strbuf, str)
            ffi.copy(keybuf, key)

            for i=0, strlen-1 do
                strbuf[i] = bit.bxor(strbuf[i], keybuf[i % keylen])
            end

            return ffi.string(strbuf, strlen)
        end,

        export = function(self)
            local settings = { }
            local blacklist = {
                watermark = 0, exploiting = 0,
                bomb = 0, hotkeys = 0, spectators = 0,
                muzzle = 0, muzzle_link = 0
            }

            local write_value = function(tbl, name, value)
                local value = value:get()

                tbl[name] = get_type(value) == 'imcolor' and
                    value:to_hex() or value
            end

            for name, value in pairs(self.general) do
                if blacklist[name] == nil then
                    if get_type(value) == 'table' then
                        for n, v in pairs(value) do
                            write_value(settings, n, v)
                        end
                    else
                        write_value(settings, name, value)
                    end
                end
            end

            clipboard.set(base64.encode( self:xorstr(json.stringify(settings)) ))

            print_raw(string.format(
                '\a%s[Solus Renewed] \aC6CBD1Successfully copied preset',
                self.general.resources.outline_color:get():to_hex():sub(1, 6)
            ))
        end,

        import = function(self)
            local success, preset = pcall(function()
                local cbdata = clipboard.get()
                return json.parse(self:xorstr(base64.decode(cbdata)))
            end)

            if success == false then
                --[[
                    print_raw(string.format(
                        '\a%s[Solus Renewed] \aFF3E3EUnable to copy preset',
                        self.general.resources.outline_color:get():to_hex():sub(1, 6)
                    ))
                ]]

                return
            end

            for name, value in pairs(preset or { }) do
                local link =
                    self.general[name] or
                    self.general.resources[name]

                if link ~= nil then
                    if tostring(link):find 'color_picker' then
                        -- ебать что это
                        value = color(value)
                    end

                    link:set(value)
                end
            end

            print_raw(string.format(
                '\a%s[Solus Renewed] \aC6CBD1Successfully pasted preset',
                self.general.resources.outline_color:get():to_hex():sub(1, 6)
            ))
        end
    }

    :struct 'smoothy' {
        to_pairs = {
            vector = { 'x', 'y', 'z' },
            imcolor =  { 'r', 'g', 'b', 'a' }
        },

        copy_tables = function(self, destination, keysTable, valuesTable)
            valuesTable = valuesTable or keysTable
            local mt = getmetatable(keysTable)

            if mt and getmetatable(destination) == nil then
                setmetatable(destination, mt)
            end

            for k,v in pairs(keysTable) do
                if type(v) == 'table' then
                    destination[k] = self:copy_tables({}, v, valuesTable[k])
                else
                    local value = valuesTable[k]

                    if type(value) == 'boolean' then
                        value = value and 1 or 0
                    end

                    destination[k] = value
                end
            end

            return destination
        end,

        resolve = function(self, easing_fn, previous, new, clock, duration)
            if type(new) == 'boolean' then new = new and 1 or 0 end
            if type(previous) == 'boolean' then previous = previous and 1 or 0 end

            local previous = easing_fn(clock, previous, new - previous, duration)

            if type(new) == 'number' then
                if math.abs(new-previous) <= .001 then
                    previous = new
                end

                if previous % 1 < 0.0001 then
                    previous = math.floor(previous)
                elseif previous % 1 > 0.9999 then
                    previous = math.ceil(previous)
                end
            end

            return previous
        end,

        perform_easing = function(self, ntype, easing_fn, previous, new, clock, duration)
            if self.to_pairs[ntype] then
                for _, key in ipairs(self.to_pairs[ntype]) do
                    previous[key] = self:perform_easing(
                        type(v), easing_fn,
                        previous[key], new[key],
                        clock, duration
                    )
                end

                return previous
            end

            if ntype == 'table' then
                for k, v in pairs(new) do
                    previous[k] = previous[k] or v
                    previous[k] = self:perform_easing(
                        type(v), easing_fn,
                        previous[k], v,
                        clock, duration
                    )
                end

                return previous
            end

            return self:resolve(easing_fn, previous, new, clock, duration)
        end,

        new = function(this, default, easing_fn)
            if type(default) == 'boolean' then
                default = default and 1 or 0
            end

            local mt = { }
            local mt_data = {
                value = default or 0,
                easing = easing_fn or function(t, b, c, d)
                    return c * t / d + b
                end
            }

            function mt.update(self, duration, value, easing)
                if type(value) == 'boolean' then
                    value = value and 1 or 0
                end

                local clock = globals.frametime
                local duration = duration or 0.15
                local value_type = get_type(value)
                local target_type = get_type(self.value)

                assert(value_type == target_type, string.format('type mismatch. expected %s (received %s)', target_type, value_type))

                if self.value == value then
                    return value
                end

                if clock <= 0 or clock >= duration then
                    if target_type == 'imcolor' or target_type == 'vector' then
                        self.value = value:clone()
                    elseif target_type == 'table' then
                        this:copy_tables(self.value, value)
                    else
                        self.value = value
                    end
                else
                    local easing = easing or self.easing

                    self.value = this:perform_easing(
                        target_type, easing,
                        self.value, value,
                        clock, duration
                    )
                end

                return self.value
            end

            return setmetatable(mt, {
                __metatable = false,
                __call = mt.update,
                __index = mt_data
            })
        end,

        new_interp = function(this, initial_value)
            return setmetatable({
                previous = initial_value or 0
            }, {
                __call = function(self, new_value, mul)
                    local mul = mul or 1
                    local tickinterval = globals.tickinterval * mul
                    local difference = math.abs(new_value - self.previous)

                    if difference > 0 then
                        local time = math.min(tickinterval, globals.frametime) / tickinterval
                        self.previous = self.previous + time * (new_value - self.previous)
                    else
                        self.previous = new_value
                    end

                    self.previous = (self.previous % 1 < .0001) and 0 or self.previous

                    return self.previous
                end
            })
        end
    }

    :struct 'draw' {
        gradient = function(self, pos, size, color1, color2, ltr)
            -- Left to right. Pass true for horizontal gradient, or false for vertical.
            local a, b = pos, pos + size

            if ltr == true then
                render.gradient(a, b, color1, color2, color1, color2)
                return
            end

            return render.gradient(a, b, color1, color1, color2, color2)
        end,

        rect_filled = function(self, pos, size, clr)
            local nWidth = size.x - pos.x
            local nHeight = size.y - pos.y
            if nWidth < 0 then
                if nHeight < 0 then
                    render.rect(size, size + vector(-nWidth, -nHeight), clr)
                else
                    local ps = vector(size.x, pos.y)

                    render.rect(ps, ps + vector(-nWidth, nHeight), clr)
                end
            else
                if nHeight < 0 then
                    local ps = vector(pos.x, size.y)
                    render.rect(ps, ps + vector(nWidth, -nHeight), clr)
                else
                    render.rect(vector(pos), pos+vector(nWidth, nHeight), clr)
                end
            end
        end,

        rect_filled_gradient = function(self, pos, size, color1, color2, bHorizontal)
            local nWidth = size.x - pos.x
            local nHeight = size.y - pos.y

            if nWidth < 0 then
                if nHeight < 0 then
                    self:gradient(size, vector(-nWidth, -nHeight), color1, color2, bHorizontal)
                else
                    self:gradient(vector(size.x, pos.y), vector(-nWidth, nHeight), color1, color2, bHorizontal)
                end
            else
                if nHeight < 0 then
                    self:gradient(vector(pos.x, size.y), vector(nWidth, -nHeight), color2, color1, bHorizontal)
                else
                    self:gradient(pos, vector(nWidth, nHeight), color2, color1, bHorizontal)
                end
            end
        end,

        rectangle = function(self, pos, size, ...)
            local a, b = pos, pos + size
            render.rect(a, b, ...)
        end,

        rect = function(self, position, size, clr, alpha, rounding, glow, outline, blur_strength)
            if size.x < 1 or size.y < 1 then
                return
            end

            position = (position + .5):floor()
            size = (size + .5):floor()

            local alpha = alpha or 1
            local rounding = math.floor(math.max(0, math.min(size.y * .25, rounding)))

            if type(blur_strength) == 'number' and blur_strength > .0 then
                render.blur(position - 1, position+size + 1, blur_strength, alpha, rounding + 1)
            end

            if get_type(glow) == 'imcolor' and glow.a > 0 then
                local glow_clr do
                    glow_clr = glow:clone()
                    glow_clr.a = glow_clr.a * alpha
                end

                render.shadow(position, position+size, glow_clr, nil, nil, rounding + 1)
            end

            -- render background
            render.rect(position, position+size, color(clr.r, clr.g, clr.b, clr.a * alpha), rounding + 1)

            -- render outline
            if type(outline) == 'table' then
                local o_color = outline.color
                local direction = outline.direction or 'top'
                local thickness = outline.thickness or 1

                if thickness >= 1 then
                    local loc = { } do
                        local cn = math.max(
                            math.abs(size.y * .5),
                            rounding * 2
                        )

                        if direction == 'top' then
                            loc = {
                                rect = { position.x + rounding, position.y - thickness, size.x - 2*rounding, thickness },
                                fade = {
                                    [1] = { position.x + size.x, position.y + rounding, thickness, cn },
                                    [2] = { position.x - thickness, position.y + rounding, thickness, cn }
                                },

                                rounding = {
                                    [1] = { position.x + rounding, position.y + rounding, 180 },
                                    [2] = { position.x + size.x - rounding, position.y + rounding, 270 }
                                }
                            }
                        end

                        if direction == 'bottom' then
                            loc = {
                                rect = { position.x + rounding, position.y + size.y, size.x - 2*rounding, thickness },
                                fade = {
                                    [1] = { position.x + size.x, position.y + size.y - rounding, thickness, -cn },
                                    [2] = { position.x - thickness, position.y + size.y - rounding, thickness, -cn }
                                },

                                rounding = {
                                    [1] = { position.x + rounding, position.y + size.y - rounding, 90 },
                                    [2] = { position.x + size.x - rounding, position.y + size.y - rounding, 0 }
                                }
                            }
                        end

                        if direction == 'left' then
                            loc = {
                                rect = { position.x - thickness, position.y + rounding, thickness, size.y - 2*rounding },
                                fade = {
                                    [1] = { position.x + rounding, position.y - thickness, cn, thickness, true },
                                    [2] = { position.x + rounding, position.y + size.y, cn, thickness, true }
                                },

                                rounding = {
                                    [1] = { position.x + rounding, position.y + size.y - rounding, 90 },
                                    [2] = { position.x + rounding, position.y + rounding, 180 },
                                }
                            }
                        end

                        if direction == 'right' then
                            loc = {
                                rect = { position.x + size.x, position.y + rounding, thickness, size.y - 2*rounding },
                                fade = {
                                    [1] = { position.x + size.x - rounding, position.y - thickness, -cn, thickness, true },
                                    [2] = { position.x + size.x - rounding, position.y + size.y, -cn, thickness, true }
                                },

                                rounding = {
                                    [1] = { position.x + size.x - rounding, position.y + size.y - rounding, 0 },
                                    [2] = { position.x + size.x - rounding, position.y + rounding, 270 },
                                }
                            }
                        end
                    end

                    if outline.style == 2 then
                        local top_or_bottom = outline.direction == 'bottom' or outline.direction == 'top'

                        local pos = {
                            { vector(loc.rect[1], loc.rect[2] + loc.rect[4] * .5), vector(loc.rect[3], loc.rect[4] * .5) },
                            { vector(loc.rect[1], loc.rect[2]), vector(loc.rect[3], loc.rect[4] * .5) }
                        }

                        if top_or_bottom then
                            pos = {
                                { vector(loc.rect[1] + loc.rect[3] * .5, loc.rect[2]), vector(loc.rect[3] * .5, loc.rect[4]) },
                                { vector(loc.rect[1], loc.rect[2]), vector(loc.rect[3] * .5, loc.rect[4]) }
                            }
                        end

                        self:gradient(
                            pos[1][1], pos[1][2],
                            color(o_color.r, o_color.g, o_color.b, o_color.a * alpha),
                            color(0, 0, 0, o_color.a * alpha * .2),
                            top_or_bottom
                        )

                        self:gradient(
                            pos[2][1], pos[2][2],
                            color(0, 0, 0, o_color.a * alpha * .2),
                            color(o_color.r, o_color.g, o_color.b, o_color.a * alpha),
                            top_or_bottom
                        )
                    else
                        self:rectangle(
                            vector(loc.rect[1], loc.rect[2]), vector(loc.rect[3], loc.rect[4]),
                            color(o_color.r, o_color.g, o_color.b, o_color.a * alpha)
                        )

                        if rounding > 0 and loc.rounding then
                            for i=1, #loc.rounding do
                                render.circle_outline(
                                    vector(loc.rounding[i][1], loc.rounding[i][2]),
                                    color(o_color.r, o_color.g, o_color.b, o_color.a * alpha),
                                    rounding+thickness, loc.rounding[i][3], .25, thickness
                                )
                            end
                        end

                        if loc.fade then
                            local dec_color do
                                dec_color = o_color * .5
                                dec_color.a = 0
                            end

                            for i=1, #loc.fade do
                                self:gradient(
                                    vector(loc.fade[i][1], loc.fade[i][2]), vector(loc.fade[i][3], loc.fade[i][4]),
                                    color(o_color.r, o_color.g, o_color.b, o_color.a * alpha), dec_color,
                                    loc.fade[i][5] or false
                                )
                            end
                        end
                    end
                end
            end

            return position, size, rounding
        end,

        graphic = function(self, pos, size, nValues_t, nMinValue, nMaxValue, nInterval, vParams_t)
            local nZero = nMinValue
            local vColor = vParams_t.clr_1

            nMinValue = 0
            nMaxValue = nMaxValue - nZero

            size.y = size.y - vParams_t.thickness

            if nMinValue == nMaxValue then
                render.line(
                    vector(pos.x, pos.y+size.y),
                    vector(pos.x + size.x, pos.y + size.y), vColor
                )

                return
            end

            local nStepX = size.x / (nInterval - 1)
            local nDelta = nMaxValue - nMinValue

            for i=1, nInterval - 1 do
                local nPercentage = {
                    (nValues_t[i] - nZero) / nDelta,
                    (nValues_t[i + 1] - nZero) / nDelta
                }

                local oData = {
                    vector(pos.x + nStepX * (i - 1), pos.y + size.y - size.y * nPercentage[1]),
                    vector(pos.x + nStepX * i, pos.y + size.y - size.y * nPercentage[2]),
                }

                for i=1, vParams_t.thickness do
                    render.line(
                        vector(oData[1].x, oData[1].y + i - 1),
                        vector(oData[2].x, oData[2].y + i - 1), vColor
                    )
                end
            end
        end,

        histogram = function(self, pos, size, nValues_t, nMinValue, nMaxValue, nInterval, vParams_t)
            local nZero = nMinValue
            nMinValue = 0
            nMaxValue = nMaxValue - nZero

            local nStepX = size.x / (nInterval - 1)
            local nDelta = nMaxValue - nMinValue

            for i=1, nInterval - 1 do
                local nPercentage = {
                    (nValues_t[i] - nZero) / nDelta,
                    (nValues_t[i + 1] - nZero) / nDelta
                }

                local oData = {
                    vector(math.floor(pos.x + nStepX * (i - 1)), math.floor(pos.y + size.y - size.y * nPercentage[1])),
                    vector(math.floor(pos.x + nStepX * i), math.floor(pos.y + size.y)),
                    isZero = math.floor(pos.y + size.y) == math.floor(pos.y + size.y - size.y * nPercentage[1])
                }

                if vParams_t.sDrawBar == 'fill' then
                    self:rect_filled(oData[1], oData[2], vParams_t.clr_1)
                elseif vParams_t.sDrawBar == 'gradient_fadeout' then
                    self:rect_filled_gradient(
                        oData[1], oData[2],
                        color(vParams_t.clr_1.r, vParams_t.clr_1.g, vParams_t.clr_1.b, 0), vParams_t.clr_1,
                        false
                    )
                elseif vParams_t.sDrawBar == 'gradient_fadein' then
                    self:rect_filled_gradient(
                        oData[1], oData[2],
                        vParams_t.clr_1, color(vParams_t.clr_1.r, vParams_t.clr_1.g, vParams_t.clr_1.b, 0),
                        false
                    )
                else

                end

                if vParams_t.bDrawPeeks and not oData.isZero then
                    self:line(oData[1], oData[2], vParams_t.clr_2)
                end
            end
        end,

        line_gradient = function(self, pos1, pos2, color1, color2, step)
            if color1.a < 5 and color2.a < 5 then
                return
            end

            local difference, step = pos2-pos1, step or 16
            local width, height = difference.x, difference.y

            local length = math.min(step, math.sqrt(width*width + height*height))
            local step_x, step_y = width / length, height / length

            for i=1, length do
                local pos1_1 = vector(pos1.x + step_x*(i-1), pos1.y + step_y*(i-1))
                local pos2_2 = vector(pos1.x + step_x*i, pos1.y + step_y*i)

                render.line(
                    pos1_1, pos2_2,
                    color1:lerp(color2, i / length)
                )
            end
        end,

        glow_line = function(self, pos1, pos2, color1, color2, color_interp, width, step, speed)
            local lerp_center = pos1:lerp(pos2, .5)

            local clr1 = color(color1.r, color1.g, color1.b, 0)

            self:line_gradient(pos1, lerp_center, clr1, color(color1.r, color1.g, color1.b, color1.a), color_interp)
            self:line_gradient(lerp_center, pos2, color(color1.r, color1.g, color1.b, color1.a), color(color2.r, color2.g, color2.b, 0), color_interp)

            local a = color1.a * .5
            local lerp_step = 1 / (width*step)

            local realtime =
                math.sin(globals.realtime * speed) * .5 + .5

            local perpendicular =
                pos1:to(pos2):cross(vector(0, 0, 1))

            for i = 1, width do
                local v1, v2 =
                    pos1 + perpendicular * i,
                    pos2 + perpendicular * i

                local v1v2center = v1:lerp(v2, .5)

                v1, v2 =
                    v1v2center:lerp(v1, 1 - lerp_step * i),
                    v1v2center:lerp(v2, 1 - lerp_step * i)

                local v3, v4 =
                    pos1 - perpendicular * i,
                    pos2 - perpendicular * i

                local v3v4center = v3:lerp(v4, .5)

                v3, v4 =
                    v3v4center:lerp(v3, 1 - lerp_step * i),
                    v3v4center:lerp(v4, 1 - lerp_step * i)

                local multiplier = 1 - math.log10(i) / math.log10(width)
                local alpha = a * multiplier

                if speed and speed > 0 then
                    alpha = alpha * realtime
                end

                local clr_step = color(color1.r, color1.g, color1.b, alpha)
                local clr_2 = color(color2.r, color2.g, color2.b, 0)

                self:line_gradient(v1, v1v2center, clr1, clr_step, color_interp)
                self:line_gradient(v1v2center, v2, clr_step, clr_2, color_interp)
                self:line_gradient(v3, v3v4center, clr1, clr_step, color_interp)
                self:line_gradient(v3v4center, v4, clr_step, clr_2, color_interp)
            end
        end
    }

    :struct 'common' {
        can_break_lc = false,

        ETP_Teleport = { 0, 0, 0, 0, 0 },
        ETP_Shifting = { 0, 0 },

        new_dragging = function(self, name, initial_pos)
            local name = tostring(name)
            local initial_pos = initial_pos or vector()

            local ui_group = ui.create 'DRAGGING\n%_#_&'
            local mi_intersection = false

            local mt = { }
            local mt_data = {
                dragging = false,
                mouse_pos = vector(0, 0),
                mouse_pos_diff = vector(0, 0),
                intersected = nil,
                mi_intersection = nil,

                reference = (function()
                    local dragging_vector = {
                        ui_group:slider(('%s:dragging_x'):format(name), -16384, 16384, initial_pos.x),
                        ui_group:slider(('%s:dragging_y'):format(name), -16384, 16384, initial_pos.y)
                    }

                    local visibility = function()
                        dragging_vector[1]:set_visible(false)
                        dragging_vector[2]:set_visible(false)
                    end

                    dragging_vector[1]:set_callback(visibility)
                    dragging_vector[1]:set_callback(visibility, true)

                    return dragging_vector
                end)()
            }

            function mt.intersects(self, mouse, pos, size)
                return
                    mouse.x >= pos.x and mouse.x <= pos.x+size.x and
                    mouse.y >= pos.y and mouse.y <= pos.y+size.y
            end

            function mt.get_position(self)
                return vector(
                    self.reference[1]:get(),
                    self.reference[2]:get()
                )
            end

            function mt.is_dragging(self)
                return self.dragging
            end

            function mt.update(self, size)
                local new_mouse_pos = ui.get_mouse_position()
                local menu_pos = ui.get_position()
                local menu_size = ui.get_size()

                local holding_key, intersection_check =
                    ui.get_alpha() > 0 and common.is_button_down(1),
                    self:intersects(new_mouse_pos, self:get_position(), size) and not
                    self:intersects(new_mouse_pos, menu_pos, menu_size)

                self.mouse_pos_diff = -(self.mouse_pos-new_mouse_pos)

                if holding_key and self.intersected == nil then
                    self.intersected = intersection_check
                end

                if holding_key and self.intersected then
                    self.dragging = true
                elseif not holding_key then
                    self.dragging = false
                    self.intersected = nil
                end

                if self.dragging then
                    local screen = render.screen_size()
                    local limit, new_pos = size * .5, vector(
                        self.reference[1]:get() + self.mouse_pos_diff.x,
                        self.reference[2]:get() + self.mouse_pos_diff.y
                    )

                    self.reference[1]:set(math.max(-limit.x, math.min(screen.x-limit.x, new_pos.x)))
                    self.reference[2]:set(math.max(-limit.y, math.min(screen.y-limit.y, new_pos.y)))
                end

                self.mouse_pos = new_mouse_pos
                self.mi_intersection = intersection_check or self.dragging

                return self:get_position(), self.dragging
            end

            local dragging = setmetatable(mt, { __index = mt_data })

            events.mouse_input:set(function()
                if dragging.mi_intersection then
                    return false
                end
            end)

            return dragging
        end
    }

local panel = new_class()
    :struct 'root' {
        top_right_offset = vector(),
        should_scale_dpi = false,

        features = {
            'loading_screen',
            'muzzle',
            'watermark', 'bomb', 'exploiting',
            'hotkeys', 'spectators'
        },

        perform = function(self, ...)
            self.top_right_offset = vector(10, 0, 0)
            self.should_scale_dpi = ctx.menu.general.resources.dpi_scale:get()

            local screen_size = render.screen_size()
            local options = ctx.menu:get_resources()

            for _, feature in ipairs(self.features) do
                local class = self[feature]

                if class and class.render then
                    class:render(screen_size, options)
                end
            end
        end,

        set_text_flags = function(self, flags)
            local flags = type(flags) == 'string' and
                flags:gsub('s', '') or ''

            if self.should_scale_dpi then
                flags = flags .. 's'
            end

            return flags
        end
    }

    :struct 'muzzle' {
        overlap = 0,
        abs_yaw = 0,
        fake_desync = 0,
        max_desync = 0,

        panel_side = true,
        panel_side_cache = nil,

        no_entry = ctx.smoothy:new_interp(),
        onshot_aa = ui.find('Aimbot', 'Ragebot', 'Main', 'Hide Shots'),
        double_tap = ui.find('Aimbot', 'Ragebot', 'Main', 'Double Tap'),

        tweens = ctx.smoothy:new({
            alpha = 0,
            panel_side = 0,
            link_alpha = 0,
            shifting = 0,
            shifting_amt = 0,
            body_yaw = 0,
            overlap = 0,
            panel_pos = nil,
        }),

        camera_move = {
            yaw = false,
            pitch = false,
            difference = 0,
            directions = vector(0, 0, 0),
            y_vector = vector(0, 1, 0),
            z_vector = vector(0, 0, 1)
        },

        native_GetAttachment = utils.get_vfunc(84, 'bool(__thiscall*)(void*, int, void*)'),
        native_GetAttachmentIDX_1stperson = utils.get_vfunc(468, 'int(__thiscall*)(void*, void*)'),
        native_GetAttachmentIDX_3rdperson = utils.get_vfunc(469, 'int(__thiscall*)(void*)'),
        native_ShouldDrawViewmodel = utils.get_vfunc(27, 'bool(__thiscall*)(void*)'),

        get_attachment = function(self, player, ret_wmodel)
            if player == nil then
                return
            end

            local weapon = player:get_player_weapon()

            if weapon == nil then
                return
            end

            local weapon_info = weapon:get_weapon_info()
            local model = ret_wmodel == true and
                weapon.m_hWeaponWorldModel or
                player.m_hViewModel[0]

            if model == nil or (not ret_wmodel and weapon_info.weapon_type == 0) then
                return
            end

            local position = ffi.new 'float[3]'
            local att_index = ret_wmodel and
                self.native_GetAttachmentIDX_3rdperson(weapon[0]) or
                self.native_GetAttachmentIDX_1stperson(weapon[0], model[0])

            if att_index > 0 and self.native_GetAttachment(model[0], att_index, position) then
                return vector(position[0], position[1], position[2])
            end
        end,

        handle_movement = function(self)
            local camera_move = self.camera_move
            local camera_angles = render.camera_angles()

            -- begin
            local me = entity.get_local_player()
            local velocity = me.m_vecVelocity
            local velocity2d = vector(velocity.x, velocity.y, 0):length() -- ???

            local difference do
                difference = globals.realtime - camera_move.difference

                if difference > globals.tickinterval*2 or math.abs(difference) > 5 then
                    camera_move.yaw = false
                end

                if camera_move.pitch == false or camera_move.yaw == false then
                    camera_move.pitch, camera_move.yaw = camera_angles:unpack()
                end

                camera_move.difference = globals.realtime
            end

            local eye_yaw_rad = math.rad(camera_angles.y)
            local eye_yaw_sin = math.sin(eye_yaw_rad)

            local forward_right_angle =
                math.atan2(eye_yaw_sin, math.cos(eye_yaw_rad)) -
                math.atan2(velocity.y, velocity.x)

            local up_angle = math.atan2(camera_angles.y, velocity.z)

            local forward_factor = velocity2d > 0 and math.cos(forward_right_angle) * velocity2d / 250 or 0
            local right_factor = velocity2d > 0 and math.sin(forward_right_angle) * velocity2d / 250 or 0
            local up_factor = math.abs(velocity.z) > 0 and math.cos(up_angle) * math.abs(velocity.z) / 250 or 0

            local vec_camera_prev_yaw = vector():angles(0, camera_move.yaw)
            local vec_camera_yaw = vector():angles(0, camera_angles.y)
            local yaw_difference_angle = math.acos(vec_camera_yaw:dot(vec_camera_prev_yaw))
            local vec_camera_yaw_cross = vec_camera_yaw:cross(vec_camera_prev_yaw)

            if vec_camera_yaw_cross:dot(camera_move.z_vector) < 0 then
                yaw_difference_angle = -yaw_difference_angle
            end

            if yaw_difference_angle ~= yaw_difference_angle then
                yaw_difference_angle = 0
            end

            local vec_camera_prev_pitch = vector():angles(camera_move.pitch, 0)
            local vec_camera_pitch = vector():angles(camera_angles.x, 0)
            local pitch_difference_angle = math.acos(vec_camera_pitch:dot(vec_camera_prev_pitch))
            local vec_camera_pitch_cross = vec_camera_pitch:cross(vec_camera_prev_pitch)

            if vec_camera_pitch_cross:dot(camera_move.y_vector) < 0 then
                pitch_difference_angle = -pitch_difference_angle
            end

            if pitch_difference_angle ~= pitch_difference_angle then
                pitch_difference_angle = 0
            end

            right_factor = right_factor + yaw_difference_angle * 32.5
            up_factor = up_factor + pitch_difference_angle * 32.5 / .5666

            camera_move.pitch = camera_angles.x
            camera_move.yaw = camera_angles.y
            camera_move.directions = vector(forward_factor, right_factor, up_factor)
        end,

        handle_antiaim = function(self)
            local me = entity.get_local_player()

            if not me then
                return
            end

            local anim_state = me:get_anim_state()

            if globals.choked_commands == 0 then
                local max_desync = rage.antiaim:get_max_desync()
                local rot1 = rage.antiaim:get_rotation()
                local rot2 = rage.antiaim:get_rotation(true)

                self.max_desync = max_desync
                self.overlap = 1 - math.clamp(math.abs(math.normalize_yaw(rot2 - rot1)), 0, max_desync) / max_desync

                self.abs_yaw = anim_state.eye_yaw
                self.fake_desync = math.normalize_yaw(rot2 - rot1)
            end
        end,

        handle_3rdperson = function(self, thirdperson)
            local value = ctx.menu.general.muzzle_panel_3rd_person:get()

            if value ~= 'Auto' then
                if thirdperson then
                    if self.panel_side_cache == nil then
                        self.panel_side_cache, self.panel_side =
                            self.panel_side, value == 'Right'
                    end
                else
                    if self.panel_side_cache ~= nil then
                        self.panel_side, self.panel_side_cache =
                            self.panel_side_cache, nil
                    end
                end
            end
        end,

        reset_movement = function(self)
            self.camera_move.pitch = false
            self.camera_move.yaw = false
            self.tweens.value.alpha = 0
        end,

        render = function(self, screen_size, options)
            local me = entity.get_local_player()
            local snapshot = lagrecord.get_snapshot(me)

            if me == nil or not me:is_alive() or snapshot == nil then
                self:reset_movement()
                return
            end

            local no_entry do
                no_entry = snapshot.command.no_entry
                no_entry = math.clamp(no_entry.x / no_entry.y, 0, 1)
                no_entry = self.no_entry(no_entry == 1 and 0 or no_entry)
            end

            local menu = ctx.menu.general
            local tweens = self.tweens.value
            local frametime = menu.muzzle_smoothing:get() * .01

            local thirdperson = common.is_in_thirdperson()
            local draw_attachment = thirdperson or self.native_ShouldDrawViewmodel(client_mode)

            if  not menu.muzzle:get() or not draw_attachment or
                (not menu.muzzle_perspective:get(1) and not thirdperson) or
                (not menu.muzzle_perspective:get(2) and thirdperson)
            then
                self:reset_movement()
                return
            end

            local muzzle_origin = self:get_attachment(me)

            if thirdperson then
                muzzle_origin = me:get_hitbox_position(2)
            end

            local screen_size = render.screen_size()
            local muzzle_screen = muzzle_origin and muzzle_origin:to_screen()

            if  muzzle_screen == nil or
                muzzle_screen.x < -100 or muzzle_screen.x > (screen_size.x + muzzle_screen.x/4) or
                muzzle_screen.y < -100 or muzzle_screen.y > (screen_size.y + muzzle_screen.y/4)
            then
                self:reset_movement()
                return
            end

            local VERIFY do
                local directions = self.camera_move.directions

                local lean_angle = math.rad(menu.muzzle_panel_lean_angle:get())
                local lean = vector(math.cos(lean_angle), math.sin(lean_angle)) * menu.muzzle_panel_lean_offset:get()
                local jiggle = vector(menu.muzzle_y_jiggle_factor:get(), menu.muzzle_z_jiggle_factor:get())

                local up_dir = thirdperson and 0 or -jiggle.y
                local right_dir = -jiggle.x * (thirdperson and .5 or 1)

                local position = vector(
                    muzzle_screen.x + (directions.y * right_dir) + (self.panel_side and lean.x or -lean.x),
                    muzzle_screen.y - lean.y - (directions.z * up_dir)
                )

                if tweens.panel_pos == nil then
                    tweens.panel_pos = position
                end

                self.tweens(.1 * frametime, {
                    panel_pos = position,
                    shifting = rage.exploit:get() == 1 and self.double_tap:get()
                })
            end

            local position = tweens.panel_pos:floor()

            local panel, size do
                size = vector(130, 53)
                size.y = size.y + math.floor(tweens.shifting * 13) -- Exploiting Indicator
                size.y = size.y + math.floor(options.rounding / 2.5) -- Rounding

                panel = vector(
                    position.x - tweens.panel_side,
                    position.y - size.y
                )
            end

            self.tweens(.15 * frametime, {
                link_alpha = menu.muzzle_link:get(),
                panel_side = self.panel_side and 0 or size.x
            })

            self.tweens(.05 * frametime, {
                alpha = 1,
                overlap = self.overlap,
                body_yaw = math.min(math.abs(self.fake_desync), self.max_desync)
            })

            if tweens.link_alpha > 0 then
                local line_color = menu.link_color:get()
                local line_options = {
                    step = menu.link_step:get(),
                    width = menu.link_width:get(),
                    interp = menu.link_interpolation:get(),
                    speed = menu.link_pulse_speed:get()
                }

                local shadow_clr = options.glow_color:clone()

                do
                    line_color.a = line_color.a * tweens.link_alpha * tweens.alpha
                    shadow_clr.a = shadow_clr.a * tweens.link_alpha * tweens.alpha
                end

                ctx.draw:glow_line(
                    muzzle_screen, position,
                    line_color, shadow_clr,
                    line_options.interp, line_options.width, line_options.step, line_options.speed * .01
                )
            end

            local THIRDPERSON_CHECK do
                -- 3rd person position check
                self:handle_3rdperson(thirdperson)

                local PSF = menu.muzzle_panel_switch_factor:get()

                local switch_limit_mpl = PSF > 150 and math.huge or PSF * .01
                local limit_left = panel.x + size.x * .5 - size.x * switch_limit_mpl
                local limit_right = panel.x + size.x * .5 + size.x * switch_limit_mpl

                if muzzle_screen.x > limit_right and self.panel_side then
                    self.panel_side = false
                end

                if muzzle_screen.x < limit_left and not self.panel_side then
                    self.panel_side = true
                end
            end

            ctx.draw:rect(
                panel, size,
                options.background_color, tweens.alpha, options.rounding, options.glow_color, {
                    style = options.rounding == 0 and 2 or 1,
                    direction = 'top',
                    thickness = options.outline_thickness,
                    color = options.outline_color
                }, options.blur * .01
            )

            local rounding = math.max(options.rounding or 4, 4)

            local TO_RIGHT, osaa_position do
                -- CIRCLES
                local radius = 8
                local camera_angles = render.camera_angles()
                local circle_position = vector(
                    panel.x + size.x - rounding*.5 - radius - 6,
                    panel.y + rounding*.5 + radius + 6
                )

                local desync = math.normalize_yaw(self.abs_yaw + self.fake_desync)

                local real_rotation = math.normalize_yaw(camera_angles.y - self.abs_yaw - 120)
                local fake_rotation = math.normalize_yaw(camera_angles.y - desync - 120)

                render.circle_outline(circle_position, color(15, 15, 15, 125 * tweens.alpha), radius, 0, 1, 2)
                render.circle_outline(circle_position, color(150, 150, 150, 220 * tweens.alpha), radius, fake_rotation, .1, 2)
                render.circle_outline(circle_position, color(45, 220, 220, 255 * tweens.alpha), radius, real_rotation, .1, 2)

                -- HIDESHOTs
                osaa_position = vector(
                    circle_position.x + radius + 2,
                    circle_position.y + (radius*2) + 4
                )

                local osaa = self.onshot_aa:get()
                local osaa_text = osaa and '\a00FF00FFON' or '\aFF0000FFOFF'

                if snapshot.command.shifting < -6 and osaa then
                    osaa_text = '\aFF7D5FFFWAIT'
                end

                render.text(2, osaa_position, color(255, 255 * tweens.alpha), 'r', 'OSAA: ', osaa_text)
            end

            local TO_LEFT do
                local offset = vector(rounding * .5, rounding * .5)
                local fraction2 = math.clamp(tweens.body_yaw, 0, 59) / 59

                render.text(2, panel + offset + 2, color(255, 255 * tweens.alpha), nil, 'ANTI-AIMBOT DEBUG')

                offset.y = offset.y + 18

                -- FAKE INDICATORS
                local clr, half_clr do
                    local fraction = math.clamp(tweens.body_yaw, 0, 30) / 30

                    if fraction >= .5 then
                        clr = color(213, 197, 84, 255):lerp(color(123, 194, 21, 255), (fraction - .5) * 2)
                    else
                        clr = color(255, 0, 0, 255):lerp(color(213, 197, 84, 255), fraction * 2)
                    end

                    clr.a = clr.a * tweens.alpha
                    half_clr = clr * .25
                    half_clr.a = 0
                end

                local text = ('FAKE (%.1f°)'):format(math.min(math.abs(self.fake_desync), self.max_desync))
                local text_size = render.measure_text(1, nil, text)

                local gradient_seg = text_size.y * .5
                ctx.draw:gradient(
                    vector(panel.x + offset.x + 3, panel.y + offset.y + 1),
                    vector(2, gradient_seg), half_clr, clr, false
                )

                ctx.draw:gradient(
                    vector(panel.x + offset.x + 3, panel.y + offset.y + 1 + gradient_seg),
                    vector(2, gradient_seg), clr, half_clr, false
                )

                render.text(1, vector(panel.x + offset.x + 9, panel.y + offset.y), color(255, 255 * tweens.alpha), '', text)

                offset.y = offset.y + 16

                -- Anti-aim overlap
                local text = 'SP: '
                local text_size = render.measure_text(2, nil, text)

                render.text(2, vector(panel.x + offset.x + 9, panel.y + offset.y), color(255, 255  * tweens.alpha), nil, text)

                ctx.draw:rect(vector(panel.x + offset.x + 9 + text_size.x + 3, panel.y + offset.y + 3), vector(20, 6), color(15, 15, 15, 220), tweens.alpha, 2)
                ctx.draw:rect(vector(panel.x + offset.x + 9 + text_size.x + 4, panel.y + offset.y + 4), vector((17-fraction2*17)+.5, 4), color(127, 153, 255, 255), tweens.alpha, 2)

                ctx.draw:rect(vector(panel.x + offset.x + 9 + text_size.x + 3 + 22, panel.y + offset.y + 3), vector(20, 6), color(15, 15, 15, 220), tweens.alpha, 2)
                ctx.draw:rect(vector(panel.x + offset.x + 9 + text_size.x + 4 + 22, panel.y + offset.y + 4), vector((17-tweens.overlap*17)+.5, 4), color(127, 153, 255, 255), tweens.alpha, 2)

                offset.y = offset.y + tweens.shifting*13

                local shifting do
                    shifting = snapshot.command.shifting

                    if rage.exploit:get() == 1 and snapshot.command.choke > 8 then
                        shifting = snapshot.command.choke
                    end

                    shifting = math.clamp(math.abs(shifting), 1, 14)
                end

                self.tweens(.15 * frametime, { shifting_amt = shifting })

                if tweens.shifting >= .685 then
                    local text = 'EXPLOITING'
                    local shadow = math.sin(globals.realtime * 3) * .5 + .5

                    local alpha = math.map(tweens.shifting, .685, 1, 0, 1) * tweens.alpha
                    local shifting_prc = (tweens.shifting_amt / 14) * 22

                    local text_color do
                        text_color = color():lerp(color(235, 95, 135), no_entry)
                        text_color.a = alpha * 220
                    end

                    render.text(2, vector(panel.x + offset.x + 9, panel.y + offset.y), text_color, nil, text)
                    render.text(2, vector(osaa_position.x - 1, panel.y + offset.y), color(255, alpha*220), 'r', shifting)

                    ctx.draw:rect(
                        vector(osaa_position.x - 37, panel.y + offset.y + 3), vector(24, 6),
                        color(15, 15, 15, 220), alpha, 2,
                        color(185, 15, 15, 255) * shadow * .75
                    )

                    ctx.draw:rect(
                        vector(osaa_position.x - 36, panel.y + offset.y + 4),
                        vector(shifting_prc < 2 and 0 or shifting_prc, 4),
                        color(127, 153, 255, 220), alpha, 2
                    )
                end
            end
        end
    }

    :struct 'watermark' {
        alpha = ctx.smoothy:new(0),
        latency = ctx.smoothy:new(0),

        get_steam_name = panorama.MyPersonaAPI.GetName,
        get_name = function(self)
            local ref = ctx.menu.general
            local name_type = ref.nickname:get()

            if name_type == 'Steam Name' then
                return self.get_steam_name()
            end

            if name_type == 'Custom' then
                local custom_name = ref.custom_name:get()

                if custom_name:gsub(' ', '') ~= '' then
                    return custom_name
                end
            end

            return common.get_username()
        end,

        get_time = function(self)
            local system_time = common.get_system_time()

            if ctx.menu.general.h12_format:get() then
                local hrs = system_time.hours % 12

                if hrs == 0 then
                    hrs = 12
                else
                    hrs = hrs < 10 and hrs or ('%02d'):format(hrs)
                end

                return ('%s:%02d%s'):format(
                    hrs,
                    system_time.minutes,
                    system_time.hours >= 12 and 'pm' or 'am'
                )
            end

            return ('%02d:%02d:%02d'):format(
                system_time.hours,
                system_time.minutes,
                system_time.seconds
            )
        end,

        get_latency = function(self)
            local netchannel = utils.net_channel()

            if netchannel == nil then
                return 0
            end

            return math.floor(netchannel.latency[1] * 1000)
        end,

        get_text = function(self, options)
            local flags = self.root:set_text_flags()

            -- PREFIX
            local prefix = ('never\a%slose  \aFFFFFFFF%s '):format(options.text_color:to_hex(), self:get_name())
            local prefix_size = render.measure_text(1, flags, prefix)

            -- PING
            local latency = globals.is_in_game and self.latency.value >= 1 and (' %dms '):format(self.latency.value) or ''
            local latency_size = render.measure_text(1, flags, latency)

            -- TIME
            local time = ' ' .. self:get_time()
            local time_size = render.measure_text(1, flags, time)

            return prefix .. latency .. time, vector(
                prefix_size.x + latency_size.x + time_size.x,
                math.max(prefix_size.y, latency_size.y, time_size.y)
            )
        end,

        render_text = function(self, id, pos, clr, flags, text, text_size)
            if id == 1 and text:find('never(.*)lose(.*)') then
                local s, e, in_text = text:find('never(.*)')

                local initial_text = 'never'
                local initial_text_size = render.measure_text(id, flags, 'never')

                render.text(id, pos - vector(text_size.x - initial_text_size.x), clr, flags, 'never')
                render.text(id, pos + vector(1), clr, flags, in_text)

                return
            end

            render.text(id, pos, clr, flags, text)
        end,

        render = function(self, screen_size, options)
            local alpha = self.alpha(.05, ctx.menu.general.watermark:get())
            local latency = self.latency(.25, self:get_latency())

            if alpha < .5 then
                return
            end

            local alpha = math.map(alpha, .5, 1, 0, 1, true)
            local offset_ins = vector(5, 4) -- TO DO [x(4) * render.get_scale]

            self.root.top_right_offset.y = self.root.top_right_offset.y + alpha * 10

            local text, text_size = self:get_text(options)
            local position = vector(
                screen_size.x - self.root.top_right_offset.x - text_size.x,
                self.root.top_right_offset.y
            )

            local _, rect_size = ctx.draw:rect(
                vector(position.x - offset_ins.x*2, position.y - 1),
                vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 + 1),
                options.background_color, alpha, options.rounding, options.glow_color, {
                    style = options.rounding == 0 and 2 or 1,
                    direction = options.outline_position,
                    thickness = options.outline_thickness,
                    color = options.outline_color
                }, options.blur * .01 * .25
            )

            self:render_text(1,
                vector(position.x + text_size.x - offset_ins.x, position.y + offset_ins.y - 1),
                color(255, 255, 255, alpha*255), self.root:set_text_flags 'r',
                text, text_size
            )

            self.root.top_right_offset.y = self.root.top_right_offset.y + alpha * (rect_size.y-2)
        end
    }

    :struct 'bomb' {
        tweens = ctx.smoothy:new({
            alpha = 0,
            planting = 0,
            planted = 0,
            defusing = 0,
            percentage = 0,
            damage_interp = 0,
            damageable = 0,
        }),

        last_site = '',
        last_beep = 0,
        last_beep_diff = 1,

        planting_site = nil,
		planting_started_at = nil,
		planting_player = nil,
		planting_time = 3.125,

        mp_c4timer = cvar.mp_c4timer,

        reset = function(self, e)
			self.planting_site = nil
			self.planting_player = nil
        end,

        beep = function(self, c)
            self.last_beep_diff = math.clamp(globals.curtime - self.last_beep, 0, 1)
            self.last_beep = globals.curtime
        end,

        begin_plant = function(self, e)
			local player_resource = entity.get_player_resource()

			if not player_resource then
				return
			end

			local center_a, center_b =
                player_resource.m_bombsiteCenterA,
                player_resource.m_bombsiteCenterB

			local site = entity.get(e.site)

			if not site then
				return
			end

			local mins, maxs =
                site.m_vecMins, site.m_vecMaxs

			local center = mins:lerp(maxs, 0.5)
			local distance_a, distance_b = center:distsqr(center_a), center:distsqr(center_b)

			self.planting_site = distance_b > distance_a and 'A' or 'B'
			self.planting_started_at = globals.curtime
			self.planting_player = entity.get(e.userid, true)

            self.last_site = self.planting_site
        end,

        damage_apply_armor = function(self, damage, armor_value)
			local armor_ratio = 0.5
			local armor_bonus = 0.5

			if armor_value > 0 then
				local flNew = damage * armor_ratio
				local flArmor = (damage - flNew) * armor_bonus

				if flArmor > armor_value then
					flArmor = armor_value * (1 / armor_bonus)
					flNew = damage - flArmor
				end

				damage = flNew
			end

			return damage
		end,

        calculate_damage = function(self, from_player, other, armor_value)
            local eye_position = from_player:get_eye_position()
            local distance = eye_position:dist(other:get_origin())

            local damage, fatal = 500, false
            local radius = damage * 3.5

            damage = damage * math.exp(-((distance * distance) / ((radius * 2 / 3) * (radius / 3))))
            damage = math.floor(self:damage_apply_armor(math.max(damage, 0), armor_value))

            return damage
        end,

        get_active_bomb = function(self, from_player)
            local curtime = globals.curtime
            local from_player = from_player or entity.get_local_player()

            local armor_value = from_player.m_ArmorValue
            local health = from_player.m_iHealth

            if self.planting_player then
                local plant_percentage = (curtime - self.planting_started_at) / self.planting_time

                if plant_percentage > 0 and plant_percentage < 1 then
                    local game_rules = entity.get_game_rules()

                    if game_rules.m_bBombPlanted == 1 then
                        return
                    end

                    -- indicator(color(252, 243, 105, 255), 'Bombsite ' .. planting_site, plant_percentage)
                    return {
                        type = 1,
                        from = from_player,
                        site = self.planting_site,
                        percentage = plant_percentage,
                        damage = self:calculate_damage(from_player, self.planting_player, armor_value)
                    }
                end
            else
                local result

                entity.get_entities('CPlantedC4', true, function(c4)
                    if c4.m_bBombDefused then
                        return
                    end

                    local explodes_at = c4.m_flC4Blow

                    local site = c4.m_nBombSite == 0 and 'A' or 'B'
                    local time_left = explodes_at - globals.curtime

                    if time_left >= 0 then
                        -- indicator(color(255, 255, 255, 200), string.format('%s - %.1fs', site, explodes_at - curtime))

                        local fatal = false
                        local damage = self:calculate_damage(from_player, c4, armor_value)

                        if from_player:is_alive() then
                            if damage >= 1 then
                                if damage >= health then
                                    fatal = true
                                    -- indicator(color(255, 0, 50, 255), 'FATAL')
                                else
                                    -- indicator(color(252, 243, 105, 255), string.format('-%d HP', damage))
                                end
                            end
                        end

                        result = {
                            type = 2,
                            entity = c4,
                            from = from_player,
                            site = site,
                            damage = damage or -1,
                            time_left = explodes_at - curtime
                        }

                        return false
                    end
                end)

                return result
            end
        end,

        render = function(self, screen_size, options)
            local me = entity.get_local_player()

            if me == nil then
                return
            end

            local menu = ctx.menu.general
            local tweens = self.tweens.value

            local defusing = 0
            local damage_alpha = 0
            local bomb = menu.bomb:get() and self:get_active_bomb(me) or nil

            do
                local frametime = globals.frametime
                local percentage = 0
                local damage_interp = 0
                local damageable = false

                if bomb then
                    if bomb.type == 1 then
                        percentage = bomb.percentage
                    end

                    if bomb.type == 2 then
                        percentage = bomb.time_left / self.mp_c4timer:int()

                        local bomb_entity = bomb.entity

                        if  bomb_entity.m_hBombDefuser ~= nil and
                            bomb_entity.m_flDefuseCountDown <= bomb_entity.m_flC4Blow
                        then
                            defusing = bomb_entity.m_flDefuseCountDown - globals.curtime
                            percentage = math.clamp(defusing, 0, 10) / 10
                        end
                    end

                    if bomb.damage >= 1 then
                        damage_interp = math.map(bomb.damage, 0, bomb.from.m_iHealth, 0, 1, true)
                    end

                    if bomb.type == 2 and bomb.from:is_alive() and bomb.damage >= 1 then
                        damageable = true
                    end
                end

                self.tweens(.1, {
                    alpha = bomb ~= nil,
                    planting = bomb ~= nil and bomb.type == 1,
                    planted = bomb ~= nil and bomb.type == 2,
                    percentage = percentage,
                    defusing = defusing > 0,
                    damage_interp = damage_interp,
                    damageable = damageable
                })
            end

            if tweens.alpha < .25 then
                return
            end

            local extra_offset = 0
            local offset_ins = vector(5, 3)
            local alpha = math.map(tweens.alpha, .25, 1, 0, 1, true)

            self.root.top_right_offset.y = self.root.top_right_offset.y + alpha * 9

            local bomb_dmg_clr = menu.bomb_dmg_color:get()
            local bomb_dmg_fatal_clr = menu.bomb_dmg_fatal_color:get()

            local damage_alpha = tweens.damageable - tweens.defusing

            if damage_alpha > 0 then
                local alpha = alpha * math.map(damage_alpha, 0.85, 1, 0, 1, true)
                local text, text_clr = '-1 HP', bomb_dmg_clr:clone()

                if bomb and bomb.damage >= 1 then
                    if bomb.damage >= bomb.from.m_iHealth then
                        text, text_clr = 'FATAL', bomb_dmg_fatal_clr:clone()
                    else
                        text = string.format('-%d HP', bomb.damage)
                    end
                end

                text_clr.a = text_clr.a * alpha

                local text_size = render.measure_text(1, self.root:set_text_flags(), text)
                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset, self.root.top_right_offset.y)

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, alpha, options.rounding, options.glow_color * .5, nil, options.blur * .01 * .25
                )

                render.text(1,
                    vector(position.x - offset_ins.x, position.y + offset_ins.y - 1),
                    text_clr, self.root:set_text_flags(), text
                )

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * damage_alpha
            end

            if tweens.defusing > 0 then
                local alpha = alpha * math.map(tweens.defusing, 0.85, 1, 0, 1, true)

                local text = string.format('DEFUSING: %.2fs', defusing)
                local text_clr = color(255, alpha * 255)

                local text_size = render.measure_text(1, self.root:set_text_flags(), text)
                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset, self.root.top_right_offset.y)

                local outline_clr = bomb_dmg_fatal_clr:lerp(bomb_dmg_clr, .75)

                if math.abs(globals.curtime - self.last_beep) <= 1.1 then
                    local diff = self.last_beep_diff * .75
                    local alpha = math.map(globals.curtime - self.last_beep, 0, diff, 1, 0, true)

                    outline_clr.a = outline_clr.a * alpha
                end

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, alpha, options.rounding, options.glow_color * .5, {
                        style = 2,
                        direction = 'bottom',
                        thickness = 1,
                        color = outline_clr
                    }, options.blur * .01 * .25
                )

                render.text(1,
                    vector(position.x - offset_ins.x, position.y + offset_ins.y - 1),
                    text_clr, self.root:set_text_flags(), text
                )

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * tweens.defusing
            end

            if tweens.planted > 0 then
                local alpha = alpha * math.map(tweens.planted, 0.25, 1, 0, 1, true)
                local text = string.format(
                    '%s: %.1fs',
                    bomb and bomb.site or self.last_site,
                    bomb and bomb.time_left or 0
                )

                local text_size = render.measure_text(1, self.root:set_text_flags(), text)
                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset, self.root.top_right_offset.y)

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, alpha, options.rounding, options.glow_color * .5, nil, options.blur * .01 * .25
                )

                local text_clr = bomb_dmg_fatal_clr:clone()

                if bomb then
                    if bomb.time_left > 5 and bomb.time_left <= 10 then
                        text_clr = bomb_dmg_clr:clone()
                    elseif bomb.time_left > 10 then
                        text_clr = color()
                    end
                end

                text_clr.a = text_clr.a * alpha

                render.text(1,
                    vector(position.x - offset_ins.x + 1, position.y + offset_ins.y - 1),
                    text_clr, self.root:set_text_flags(), text
                )

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * tweens.planted
            end

            if tweens.planting > 0 then
                local alpha = alpha * math.map(tweens.planting, 0.25, 1, 0, 1, true)
                local text = 'Bombsite: ' .. self.last_site

                local text_size = render.measure_text(1, self.root:set_text_flags(), text)
                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset, self.root.top_right_offset.y)

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, alpha, options.rounding, options.glow_color * .5, nil, options.blur * .01 * .25
                )

                render.text(1,
                    vector(position.x - offset_ins.x, position.y + offset_ins.y - 1),
                    color(255, alpha * 255), self.root:set_text_flags(), text
                )

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * tweens.planting
            end

            if tweens.percentage > 0 then
                local alpha = math.map(alpha, .75, 1, 0, 1, true)
                local text_size = render.measure_text(1, self.root:set_text_flags(), 'o')

                text_size.x = text_size.y * .85

                local position = vector(
                    screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset,
                    self.root.top_right_offset.y
                )

                local outline_color = color(255, 0):lerp(menu.bomb_dst_color:get(), tweens.damage_interp)

                rect_pos, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, alpha, options.rounding, options.glow_color * .5, {
                        direction = 'left',
                        style = options.rounding == 0 and 2 or 1,
                        thickness = options.rounding == 0 and 2 or 1,
                        color = outline_color
                    }, options.blur * .01 * .25
                )

                local radius = rect_size.y*.35 - 1
                local thickness = (text_size.y / 11.5) * 1.25
                local circle_position = rect_pos + (rect_size * .5)

                render.circle_outline(
                    circle_position, color(15, 15, 15, 220 * alpha),
                    radius, 0, 1, thickness
                )

                render.circle_outline(
                    circle_position, color(255, alpha * 200),
                    radius, 0, tweens.percentage, thickness
                )

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * alpha
            end

            self.root.top_right_offset.y = self.root.top_right_offset.y + alpha * (rect_size.y-1)
        end
    }

    :struct 'exploiting' {
        lagcomp = ctx.smoothy:new({ 0, 0, 0, 0, 0 }),
        tweens = ctx.smoothy:new({
            alpha = 0,
            fakelag_alpha = 0,
            lagcomp_alpha = 0,
            shifting_alpha = 0,
        }),

        no_entry = ctx.smoothy:new_interp(),
        lc_broken = ctx.smoothy:new_interp(),

        render = function(self, screen_size, options)
            local tweens = self.tweens.value
            local menu = ctx.menu.general

            local me = entity.get_local_player()

            if me == nil then
                return
            end

            local snapshot = lagrecord.get_snapshot(me)
            local no_entry, teleport do
                no_entry = snapshot ~= nil and snapshot.command.no_entry or vector(0, 0)
                no_entry = math.clamp(no_entry.x / no_entry.y, 0, 1)

                no_entry = self.no_entry(no_entry == 1 and 0 or no_entry)
                teleport = self.lc_broken((snapshot and snapshot.origin.change > 4096) and 1 or 0, 4)
            end

            do
                local ETP = ctx.common.ETP_Teleport

                self.lagcomp(.05, ETP)
                self.tweens(.05, {
                    alpha = menu.exploiting:get() and me:is_alive(),
                    lagcomp_alpha = ETP[3] > 0 and ETP[4] > 0 and ETP[5] > 0,
                    fakelag_alpha = rage.exploit:get() == 1 or ctx.common.can_break_lc,
                    shifting_alpha = rage.exploit:get() == 1 or math.min(unpack(ctx.common.ETP_Shifting)) < -6
                })
            end

            if tweens.alpha < .25 then
                return
            end

            local extra_offset = 0
            local offset_ins = vector(5, 3)
            local alpha =
                math.map(tweens.alpha, .25, 1, 0, 1, true) *
                math.max(tweens.shifting_alpha, tweens.lagcomp_alpha, tweens.fakelag_alpha)

            self.root.top_right_offset.y = self.root.top_right_offset.y + alpha * 9

            -- EXPLOITING INDICATOR
            local rect_size = vector()
            local lcb_color = menu.exploiting_lcb_color:get()

            if tweens.shifting_alpha > 0 then
                local text = 'EXPLOITING'
                local text_size = render.measure_text(1, self.root:set_text_flags(), text)

                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x, self.root.top_right_offset.y)
                local rect_alpha = alpha * tweens.shifting_alpha

                rect_alpha =
                    rect_alpha < .75 and 0 or
                    math.map(rect_alpha, .75, 1, 0, 1)

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, rect_alpha, options.rounding, options.glow_color * .5, {
                        style = 2,
                        direction = 'bottom',
                        thickness = 1,
                        color = menu.exploiting_shifting_color:get()
                    }, options.blur * .01 * .25
                )

                local text_color = color():lerp(menu.exploiting_noentry_color:get(), no_entry)

                text_color.a = rect_alpha * 255

                render.text(1, vector(position.x - offset_ins.x, position.y + offset_ins.y - 1), text_color, self.root:set_text_flags(), text)

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * tweens.shifting_alpha
            end

            -- GRAPHICS
            if tweens.lagcomp_alpha > 0 then
                local placeholder = string.rep('\x20', 9)
                local text_size = render.measure_text(1, self.root:set_text_flags(), placeholder)
                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset, self.root.top_right_offset.y)

                local rect_alpha = alpha * tweens.lagcomp_alpha

                rect_alpha =
                    rect_alpha < .5 and 0 or
                    math.map(rect_alpha, .5, 1, 0, 1)

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, rect_alpha, options.rounding, options.glow_color * .5, nil, options.blur * .01 * .25
                )

                local etp_data = self.lagcomp.value
                local graph_color do
                    graph_color = lcb_color:clone()
                    graph_color.a = graph_color.a * rect_alpha
                end

                ctx.draw:graphic(
                    vector(position.x - offset_ins.x, position.y + offset_ins.y*2 - 1),
                    vector(text_size.x, text_size.y - offset_ins.y),
                    etp_data, 0, 1000, #etp_data, {
                        thickness = 1,
                        clr_1 = graph_color
                    }
                )

                extra_offset = extra_offset +
                    (text_size.x + offset_ins.x*2 + offset_ins.x) * tweens.lagcomp_alpha
            end

            -- FAKELAG INDICATOR
            if tweens.fakelag_alpha > 0 then
                local text = ('FL: %s%d'):format(
                    snapshot and snapshot.command.choke < 10 and string.rep('\x20', 2) or '',
                    snapshot and snapshot.command.choke or -1
                )

                local text_size = render.measure_text(1, self.root:set_text_flags(), text)
                local position = vector(screen_size.x - self.root.top_right_offset.x - text_size.x - extra_offset, self.root.top_right_offset.y)

                local clr = menu.exploiting_lcu_color:get():lerp(lcb_color, teleport)

                _, rect_size = ctx.draw:rect(
                    vector(position.x - offset_ins.x*2, position.y),
                    vector(text_size.x + offset_ins.x*2, text_size.y + offset_ins.y*2 - 1),
                    options.background_color, alpha * tweens.fakelag_alpha, options.rounding, options.glow_color * .5, {
                        direction = 'left',
                        style = options.rounding == 0 and 2 or 1,
                        thickness = options.rounding == 0 and 2 or 1,
                        color = clr
                    }, options.blur * .01 * .25
                )

                render.text(1,
                    vector(position.x - offset_ins.x, position.y + offset_ins.y - 1),
                    color(255, alpha * tweens.fakelag_alpha * 255), self.root:set_text_flags(), text
                )
            end

            self.root.top_right_offset.y = self.root.top_right_offset.y + alpha * (rect_size.y-1)
        end
    }

    :struct 'hotkeys' {
        active = { },

        alpha = ctx.smoothy:new(0),
        holding = ctx.smoothy:new(1),
        width = ctx.smoothy:new(110),

        dragging = ctx.common:new_dragging('Hotkeys', vector(50, 450)),

        to_sentence = function(self, should_use, text)
            if should_use ~= true then
                return text
            end

            return text:sub(1,1):upper() .. text:sub(2):lower()
        end,

        get_value = function(self, mode, value)
            local value_type = type(value)

            if value_type == 'boolean' then
                if value == false then
                    value = 'disabled'
                elseif mode == 1 then
                    value = 'holding'
                else
                    value = 'toggled'
                end
            end

            if value_type == 'table' then
                local new_tbl = { }

                if #value <= 1 then
                    new_tbl = value
                else
                    for _, current in ipairs(value) do
                        local cur_value = ''

                        current:gsub('%w+', function(str)
                            cur_value = cur_value .. str:sub(1, 1)
                        end)

                        new_tbl[#new_tbl+1] = cur_value:upper()
                    end
                end

                value = table.concat(new_tbl, ', ')
            end

            return value
        end,

        get_count = function(self, tbl)
            local count = 0

            for _, value in pairs(tbl or { }) do
                if value ~= nil then
                    count = count + 1
                end
            end

            return count
        end,

        handle = function(self)
            local existent_keys = { }
            local active_keys = self.active

            local all_active = false
            local name_width, value_width = 0, 0

            local flags = self.root:set_text_flags()
            local use_sentence = ctx.menu.general.hotkeys_sentence:get()

            for _, hotkey in pairs(ui.get_binds()) do
                local unique_id = hotkey.reference:get_id()
                local value = self:get_value(hotkey.mode, hotkey.value)
                local name = self:to_sentence(use_sentence, hotkey.name)

                if hotkey.active then
                    all_active = true
                    existent_keys[unique_id] = hotkey
                end

                active_keys[unique_id] = active_keys[unique_id] or {
                    alpha = ctx.smoothy:new(0),
                    height = 0,
                    name_width = 0,
                    value_width = 0,

                    name = name,
                    mode = hotkey.mode,
                    value = hotkey.value,
                    reference = hotkey.reference
                }

                local val = active_keys[unique_id] do
                    val.name = name
                    val.value = value
                    val.mode = hotkey.mode
                    val.reference = hotkey.reference

                    local name_size = render.measure_text(1, flags, name)
                    local value_size = render.measure_text(1, flags, string.format('[%s]', value))

                    val.height = math.max(name_size.y, value_size.y)
                    val.name_width = name_size.x
                    val.value_width = value_size.x
                end
            end

            for id, hotkey in pairs(active_keys) do
                local this = active_keys[id]
                local active = existent_keys[id] ~= nil

                this.alpha(
                    (not all_active or active) and .15 or .1,
                    active
                )

                if this.alpha.value <= 0 then
                    active_keys[id] = nil
                elseif this.alpha.value >= .25 or active then
                    if name_width < this.name_width then
                        name_width = this.name_width
                    end

                    if value_width < this.value_width then
                        value_width = this.value_width
                    end
                end
            end

            return active_keys, all_active, name_width, value_width
        end,

        render = function(self, screen_size, options)
            local me = entity.get_local_player()

            if me == nil then
                return
            end

            local hotkeys, all_active, name_width, value_width = self:handle()

            local menu_check = ui.get_alpha() == 1 or (self:get_count(hotkeys) > 0 and all_active)
            local can_show_hotkeys = ctx.menu.general.hotkeys:get() and menu_check

            self.alpha(.05, can_show_hotkeys)
            self.holding(.05, can_show_hotkeys and self.dragging:is_dragging() and .6 or 1)

            if self.alpha.value <= 0 then
                return
            end

            local alpha = self.alpha.value
            local offset_ins = vector(10, 5)

            local keyval_gap = 15
            local keyval_rounding = math.floor(options.rounding * .5)

            -- header
            local position = self.dragging:get_position()

            local text = 'keybinds'
            local text_size = render.measure_text(1, self.root:set_text_flags(), text)
            local text_size_base = render.measure_text(1, '', text)

            self.width(.05, math.max(
                offset_ins.x * 4 + text_size.x * (text_size.y / text_size_base.y),
                math.max(110, keyval_rounding * 2 + name_width + value_width + keyval_gap)
            ))

            local max_width = math.floor(self.width.value + .85)
            local rect_size = vector(max_width, text_size.y + offset_ins.y*2)

            ctx.draw:rect(
                position, rect_size,
                options.background_color, alpha * self.holding.value, options.rounding, options.glow_color, {
                    style = options.rounding == 0 and 2 or 1,
                    direction = 'top',
                    thickness = options.outline_thickness,
                    color = options.outline_color
                }, options.blur * .01 * .25
            )

            render.text(1,
                vector(position.x + rect_size.x * .5 - text_size.x * .5, position.y + offset_ins.y-1),
                color(255, alpha * self.holding.value * 255),
                self.root:set_text_flags(), text
            )

            -- contents
            local offset = 2

            for id, hotkey in pairs(hotkeys) do
                local alpha = alpha * hotkey.alpha.value
                local text_size, text_alpha = vector(hotkey.name_width, hotkey.height), 0

                if alpha >= .25 then
                    text_alpha = math.min(1, math.map(alpha, .25, 1, 0, 1.2))
                end

                local e_offset = -(6 - math.floor(text_size.y * .5 * alpha))
                local text_position = vector(position.x + keyval_rounding, position.y + rect_size.y + 4 + offset + e_offset)

                local value_start = rect_size.x - hotkey.value_width
                local value_alpha = math.map(value_start-hotkey.name_width, 0, keyval_gap, 0, 1, true)

                local h_alpha = text_alpha * self.holding.value

                render.text(1, text_position, color(255, 255 * h_alpha), self.root:set_text_flags(), hotkey.name)
                render.text(1,
                    text_position + vector(rect_size.x - keyval_rounding*2), color(255, 255 * h_alpha * value_alpha),
                    self.root:set_text_flags 'r', ('[%s]'):format(hotkey.value)
                )

                offset = offset + text_alpha * (text_size.y + text_size.y * .35)
            end

            self.dragging:update(rect_size)
        end
    }

    :struct 'spectators' {
        active = { },

        alpha = ctx.smoothy:new(0),
        holding = ctx.smoothy:new(1),
        width = ctx.smoothy:new(90),

        dragging = ctx.common:new_dragging('Spectators', vector(50, 500)),

        get_count = function(self, tbl)
            local count = 0

            for _, value in pairs(tbl or { }) do
                if value ~= nil then
                    count = count + 1
                end
            end

            return count
        end,

        get_spectators = function(self)
            local spectators = { }
            local me, target = entity.get_local_player()

            if me ~= nil then
                if me.m_hObserverTarget and (me.m_iObserverMode == 4 or me.m_iObserverMode == 5) then
                    target = me.m_hObserverTarget
                else
                    target = me
                end

                entity.get_players(false, false, function(player)
                    if player:is_alive() then
                        return
                    end

                    local obtarget = player.m_hObserverTarget
                    local obmode = player.m_iObserverMode

                    if obtarget and obtarget == target and player ~= me and (obmode == 4 or obmode == 5) then
                        spectators[#spectators+1] = player
                    end
                end)
            end

            return spectators
        end,

        handle = function(self)
            local existent_keys = { }
            local active_keys = self.active

            local all_active = false
            local max_width = 0
            local max_height = 0

            local flags = self.root:set_text_flags()

            for _, spectator in pairs(self:get_spectators()) do
                local unique_id = spectator:get_index()
                local name = spectator:get_name()

                if #name > 16 then
                    name = name:sub(0, 16) .. '..'
                end

                existent_keys[unique_id], all_active = spectator, true

                active_keys[unique_id] = active_keys[unique_id] or {
                    name = name,
                    size = vector(),
                    entity = spectator,
                    avatar = spectator:get_steam_avatar(),
                    alpha = ctx.smoothy:new(0)
                }

                local val = active_keys[unique_id] do
                    val.name = name
                    val.entity = spectator
                    val.avatar = spectator:get_steam_avatar()
                    val.size = render.measure_text(1, flags, name)
                end
            end

            for id, hotkey in pairs(active_keys) do
                local this = active_keys[id]
                local active = existent_keys[id] ~= nil

                this.alpha(
                    (not all_active or active) and .15 or .1,
                    active
                )

                if this.alpha.value <= 0 then
                    active_keys[id] = nil
                elseif this.alpha.value >= .25 or active then
                    if max_width < this.size.x then
                        max_width = this.size.x
                        max_height = this.size.y
                    end
                end
            end

            return active_keys, all_active, max_width, max_height
        end,

        render = function(self, screen_size, options)
            local me = entity.get_local_player()

            if me == nil then
                return
            end

            local spectators, all_active, spec_width, spec_height = self:handle()

            local menu_check = ui.get_alpha() == 1 or (self:get_count(spectators) > 0 and all_active)
            local can_show_spectators = ctx.menu.general.spectators:get() and menu_check

            self.alpha(.05, can_show_spectators)
            self.holding(.05, can_show_spectators and self.dragging:is_dragging() and .6 or 1)

            if self.alpha.value <= 0 then
                return
            end

            local alpha = self.alpha.value
            local offset_ins = vector(10, 5)

            local keyval_gap = spec_height / 2
            local keyval_rounding = math.floor(options.rounding * .5)

            -- header
            local position = self.dragging:get_position()

            local text = 'spectators'
            local text_size = render.measure_text(1, self.root:set_text_flags(), text)
            local text_size_base = render.measure_text(1, '', text)

            self.width(.05, math.max(
                offset_ins.x * 4 + text_size.x * (text_size.y / text_size_base.y),
                math.max(90, keyval_rounding * 2 + spec_width + keyval_gap + spec_height)
            ))

            local max_width = math.floor(self.width.value + .85)
            local rect_size = vector(max_width, text_size.y + offset_ins.y*2)

            ctx.draw:rect(
                position, rect_size,
                options.background_color, alpha * self.holding.value, options.rounding, options.glow_color, {
                    style = options.rounding == 0 and 2 or 1,
                    direction = 'top',
                    thickness = options.outline_thickness,
                    color = options.outline_color
                }, options.blur * .01 * .25
            )

            render.text(1,
                vector(position.x + rect_size.x * .5 - text_size.x * .5, position.y + offset_ins.y-1),
                color(255, alpha * self.holding.value * 255),
                self.root:set_text_flags(), text
            )

            -- contents
            local offset = 2
            local avatar_height = (vector(spec_height, spec_height) - 2):floor()

            for id, spectator in pairs(spectators) do
                local alpha = alpha * spectator.alpha.value
                local text_size, text_alpha = spectator.size, 0

                if alpha >= .25 then
                    text_alpha = math.min(1, math.map(alpha, .25, 1, 0, 1.2))
                end

                local e_offset = -(6 - math.floor(text_size.y * .5 * alpha))
                local text_position = vector(position.x + keyval_rounding, position.y + rect_size.y + 4 + offset + e_offset)

                local h_alpha = text_alpha * self.holding.value
                local pf_color = color(255, 255 * h_alpha)

                if spectator.avatar then
                    render.texture(spectator.avatar, text_position + vector(2, 2), avatar_height, pf_color, 'f', keyval_rounding)

                    text_position = text_position + vector(spec_height + keyval_gap)
                end

                render.text(1, text_position, pf_color, self.root:set_text_flags(), spectator.name)

                offset = offset + text_alpha * (text_size.y + text_size.y * .4)
            end

            self.dragging:update(rect_size)
        end
    }

-- CLASS_END
local INITIALIZATION do
    ctx.menu:initialize() -- initialize menu elements

    events.render:set(function() panel.root:perform() end)
    events.createmove:set(function(c) panel.muzzle:handle_movement() end)

    events.entity_update:set(function(c)
        if c.entity ~= entity.get_local_player() then
            return
        end

        local snapshot = lagrecord.get_snapshot(c.entity)

        if snapshot == nil then
            return
        end

        local ETP_Teleport, ETP_Shifting =
            ctx.common.ETP_Teleport, ctx.common.ETP_Shifting

        for i=1, #ETP_Shifting do
            ETP_Shifting[i] = i == #ETP_Shifting and
            snapshot.command.shifting or ETP_Shifting[i+1]
        end

        for i=1, #ETP_Teleport do
            local teleport_value = math.map(snapshot.origin.change, 4096, 6144, 0, 1000, true)

            ETP_Teleport[i] = i == #ETP_Teleport and
                math.floor(teleport_value) or ETP_Teleport[i+1]
        end
    end)

    events.net_update_end:set(function()
        local me = entity.get_local_player()
        local snapshot = lagrecord.get_snapshot(me)

        ctx.common.can_break_lc = false

        panel.muzzle:handle_antiaim()

        if me and me:is_alive() and snapshot then
            local velocity = me.m_vecVelocity

            if velocity:length2d() > 250 or velocity.z > 250 or snapshot.origin.change > 4096 then
                ctx.common.can_break_lc = true
            end
        end
    end)

    events.bomb_beginplant:set(function(e) panel.bomb:begin_plant(e) end)
    events.bomb_abortplant:set(function() panel.bomb:reset() end)
    events.bomb_planted:set(function() panel.bomb:reset() end)
    events.round_start:set(function() panel.bomb:reset() end)

    events.emit_sound:set(function(c)
        if c.sound_name:find 'weapons/c4/c4_beep' then
            panel.bomb:beep(c)
        end
    end)
end
