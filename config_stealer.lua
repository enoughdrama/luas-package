local clipboard = require 'neverlose/clipboard'
local inspect = require 'neverlose/inspect'

local configsData = db.tab_mover or {  }
local menu_tabs = {
    ['Aimbot'] = {
        'Ragebot',
        'Anti Aim',
        'Legitbot'
    },

    ['Visuals'] = {
        'Players',
        'World',
        'Inventory'
    },

    ['Miscellaneous'] = {
        'Main',
        'Scripts'
    }
}

local update_list = function(tbl)
    local configs_list = {  }

    for n,v in pairs(tbl) do
        local content = json.parse(v)

        table.insert(configs_list, n)
    end

    return configs_list
end

local contains = function(name, group)
    for n,v in pairs(group) do
        if n == name or v == name then
            return true
        end
    end

    return false
end

local system do
    system = {  }

    system.parse_remote = function(self, url)
        network.get(url, {}, function(content)
            local success, data = pcall(function()
                return json.parse(content)
            end)

            if not success then
                print_error 'url parser error'
                return
            end

            for n,v in pairs(configsData) do

                configsData[n] = v
            end

            return true
        end)
    end

    system.export = function(self, name)
        local name = name or 'Last Config'
        local exportData = {  }

        for tab,refs in pairs(menu_tabs) do
            exportData[tab] = {  }

            for idx,ref in ipairs(refs) do
                local group = ui.find(tab, ref)
                exportData[tab][ref] = group:export()
            end
        end

        exportData = json.stringify({
            config = exportData
        })

        configsData[name] = exportData
        return exportData
    end

    system.import = function(self, data, whitelist)
        local success, data = pcall(function()
            return json.parse(data)
        end)

        if not success then
            print_error 'An error occured: provided config data is outdated'
            return
        end

        for tab,refs in pairs(menu_tabs) do
            local config = data.config[tab]

            if config ~= nil then
                for _,ref in pairs(refs) do
                    if contains(ref, config) and contains(ref, whitelist) then
                        local was_success, group = pcall(function()
                            return ui.find(tab, ref)
                        end)

                        if not was_success then
                            print_error 'An error occured: skipped [%s=>%s] - outdated'
                            goto continue end

                        group:import(data.config[tab][ref])
                    end

                    ::continue::
                end
            end
        end
    end

    system = setmetatable({}, {
        __index = system,
        __metatable = false
    })
end

local menu do
    menu = {  }

    local make_group = function(tab, name, callback)
        local group = ui.create(tab, name)

        callback(menu, group)
    end

    make_group('General', 'Sharing', function(th, group)
        th.sharing = {  }
        local th = th.sharing

        local configs_list = update_list(configsData)

        th.configs_list = group:list('Avaliable Configs', configs_list)
        th.config_name = group:input 'Name: '

        th.import_btn = group:button('Import', function()
            local section = th.configs_list:list()[th.configs_list:get()]
            local config_data = configsData[section]

            assert(config_data ~= nil, 'Your config is not found')

            local whitelist = {  }

            for _,v in pairs(menu.mover_tabs:list()) do
                if menu.mover_tabs:get(v) then
                    table.insert(whitelist, v)
                end
            end

            system:import(config_data, whitelist)
        end, true)
        th.export_btn = group:button('Export', function()
            local config_data = system:export(th.config_name:get())

            th.configs_list:update(
                update_list(configsData)
            )
        end, true)

        th.configs_list:set_callback(function(self)
            local section = self:list()[self:get()]

            if section == nil then return end
            th.config_name:set(section)
        end, true)
    end)

    make_group('General', 'Tab Mover', function(th, group)
        local show_items = {  }

        for _,ref in pairs(menu_tabs) do

            for _,item in pairs(ref) do
                table.insert(show_items, item)
            end
        end

        th.mover_tabs = group:listable('Groups: ', show_items)
        th.export_btn = group:button('Export', function()
            local config_data = system:export()

            clipboard.set(config_data)

            menu.sharing.configs_list:update(
                update_list(configsData)
            )
        end, true)
        th.import_btn = group:button('Import', function()
            local config_data = clipboard.get()
            local whitelist = {  }

            for _,v in pairs(th.mover_tabs:list()) do
                if th.mover_tabs:get(v) then
                    table.insert(whitelist, v)
                end
            end

            system:import(config_data, whitelist)
        end, true)
    end)
end

events.shutdown:set(function()
    db.tab_mover = configsData
end)
