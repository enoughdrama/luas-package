local vector = require 'vector'

local cached_items = {}
local old_ui = ui

function math.clamp(x, min, max)
    return math.max(math.min(x, max), min)
end

local function lerp(a, b, t)
    return a * (1 - t) + b * t
end

local function angle_to_forward(angle_x, angle_y)
    local sy = math_sin(math_rad(angle_y));
    local cy = math_cos(math_rad(angle_y));
    local sp = math_sin(math_rad(angle_x));
    local cp = math_cos(math_rad(angle_x));
    return cp * cy, cp * sy, -sp
end

local color
do
    local lerp_color = function(old, new, fraction, color)
        local diff = math.abs(old[color] - new[color])

        return math.clamp(old[color] + (old[color] < new[color] and diff * fraction or -diff * fraction), 0, 255)
    end

    local methods = {
        lerp = function(self, color, fraction)
            return {
                r = lerp_color(self, color, fraction, 'r'),
                g = lerp_color(self, color, fraction, 'g'),
                b = lerp_color(self, color, fraction, 'b'),
                a = lerp_color(self, color, fraction, 'a')
            }
        end,

        alpha_modulate = function(self, new_alpha)
            self.a = math.clamp(new_alpha, 0, 255)

            return self
        end
    }

    color = setmetatable({}, {
        __call = function(mt_data, r, g, b, a)
            return setmetatable({
                r = math.clamp(r or 255, 0, 255),
                g = math.clamp(g or 255, 0, 255),
                b = math.clamp(b or 255, 0, 255),
                a = math.clamp(a or 255, 0, 255)
            }, {
                __index = methods,
                __metatable = 'top-secret',
                __tostring = function()
                    return string.format('color(%i, %i, %i, %i)', r or 255, g or 255, b or 255, a or 255)
                end
            })
        end
    })
end

local assign_mt = function(element, init_value, type)
    local methods = {
        type = type,

        override_info = {
            is_overriden = false,
            override_value = nil
        },

        override = function(self, v)
            self.override_info = {
                is_overriden = true,
                override_value = v
            }
        end,
        get = function(self, value)
            if type == 'imcolor' then
                local r, g, b, a = ui.get(element)

                return color(r, g, b, a)
            end

            return self.override_info.is_overriden and self.override_info.override_value or ui.get(element, value)
        end,
        set = function(self, c)
            if string.find(tostring(c), 'color') then
                ui.set(element, c.r, c.g, c.b, c.a)
            end

            return ui.set(element, c)
        end,
        get_name = function(self)
            return ui.name(element)
        end,
        set_visible = function(self, c)
            return ui.set_visible(element, c)
        end,
        set_callback = function(self, callback, force_init)
            if force_init then
                callback()
            end

            return ui.set_callback(element, callback)
        end,
        get_list = function(self, value)
            for n, v in pairs(ui.get(element)) do
                if value == v then
                    return true
                end
            end

            return false
        end,
        update = function(self, items)
            return ui.update(element, items)
        end,
        reset = function(self)
            return old_ui.set(element, init_value or false)
        end
    }

    local userdata = newproxy(true)
    local metatable = getmetatable(userdata)

    metatable.__index = methods
    metatable.__metatable = 'attempt to call unreachable method'
    metatable.__tostring = function()
        return string.format('menu_item(%s)', ui.name(element))
    end

    return userdata
end

local ui_data = {
    tab = setmetatable({}, {
        __call = function(mt_data, tab, group)
            local features = {
                switch = function(self, name, init)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_checkbox(tab, group, name)
                    ))

                    return cached_items[#cached_items]
                end,

                slider = function(self, name, min, max, default, show_tooltip, unit, scale, tooltips)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_slider(tab, group, name, min, max, default, show_tooltip, unit, scale or 1, tooltips),
                        default
                    ))

                    return cached_items[#cached_items]
                end,

                combo = function(self, name, box)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_combobox(tab, group, name, box), box
                    ))

                    return cached_items[#cached_items]
                end,

                selectable = function(self, name, items)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_multiselect(tab, group, name, items)
                    ))

                    return cached_items[#cached_items]
                end,

                button = function(self, name, callback)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_button(tab, group, name, callback)
                    ))

                    return cached_items[#cached_items]
                end,

                color_picker = function(self, name, color)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_color_picker(tab, group, name, color.r, color.g, color.b, color.a), nil, 'imcolor'
                    ))

                    return cached_items[#cached_items]
                end,

                hotkey = function(self, name, inline, default)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_hotkey(tab, group, name, inline, default), default
                    ))

                    return cached_items[#cached_items]
                end,

                label = function(self, name)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_label(tab, group, name), name
                    ))

                    return cached_items[#cached_items]
                end,

                list = function(self, name, items)
                    table.insert(cached_items, assign_mt(
                        old_ui.new_listbox(tab, group, name, items), items
                    ))

                    return cached_items[#cached_items]
                end,
            }

            local userdata = newproxy(true)
            local metatable = getmetatable(userdata)

            metatable.__index = features
            metatable.__name = group
            metatable.__tostring = function() return string.format("menu_group(%s, %s)", tab, group) end
            metatable.__metatable = 'attempt to call unreachable method'

            return userdata

            --return setmetatable(features, {__name = group, __tostring = function() return string.format("menu_group(%s, %s)", tab, group) end, __metatable = 'attempt to call unreachable method'})
        end
    }),

    find = function(tab, group, name)
        local a, b = old_ui.reference(tab, group, name)

        if not b then
            return assign_mt(a)
        else
            return assign_mt(a), assign_mt(b)
        end
    end,

    is_menu_open = function()
        return ui.is_menu_open
    end,
    menu_position = function()
        local userdata = newproxy(true)
        local x, y = ui.get_menu_position()

        getmetatable(userdata).__index = { x = x, y = y }
        return userdata
    end,
    menu_size = function()
        local userdata = newproxy(true)
        local x, y = ui.menu_size()

        getmetatable(userdata).__index = { x = x, y = y }
        return userdata
    end
}

local ui = newproxy(true); getmetatable(ui).__index = ui_data
client.set_event_callback('paint_ui', function()
    local tickcount = globals.tickcount()

    if tickcount % 2 ~= 0 then
        return
    end

    for _, _ in pairs(cached_items) do
        cached_items.override_info = {
            is_overriden = false,
            override_value = nil
        }
    end
end)

local render_data = {
    screen_size = function()
        local w, h = client.screen_size()

        return { x = w, y = h }
    end,

    camera_position = function()
        local x, y = client.camera_position()

        return { x = x, y = y }
    end,

    camera_angles = function()
        local x, y = client.camera_angles()

        return { x = x, y = y }
    end,

    world_to_screen = function(vec)
        local rx, ry = render.world_to_screen(vec.x, vec.y, vec.z)

        return {
            x = rx,
            y = ry
        }
    end,

    get_scale = function()
        return ui.find('Miscellaneous', 'SETTINGS', 'DPI Scale'):get()
    end,

    indicator = function(color, ...)
        return renderer.indicator(color.r, color.g, color.b, color.a, ...)
    end,

    load_image = function(format, buffer, size)
        if format == 'RGBA' then
            return renderer.load_rgba(buffer, size.x, size.y)
        end
    end,

    measure_text = function(flags, ...)
        local userdata = newproxy(true)
        local x, y = render.measure_text(flags, ...)

        getmetatable(userdata).__index = { x = x, y = y }
        return userdata
    end,

    text = function(position, clr, flags, max_width, ...)
        local r, g, b, a = clr.r, clr.g, clr.b, clr.a

        return renderer.text(position.x, position.y, r, g, b, a, flags, max_width, ...)
    end,

    line = function(position, position2, clr)
        local r, g, b, a = clr.r, clr.g, clr.b, clr.a

        return renderer.line(position.x, position.y, position2.x, position2.y, r, g, b, a)
    end

}

local render = newproxy(true); getmetatable(render).__index = render_data

return {
    ['render'] = render,
    ['ui'] = ui,
    ['color'] = color
}
