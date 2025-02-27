local dt = {ui.reference('RAGE', 'Other', 'Double Tap')}

client.set_event_callback('paint', function()
    local players = entity.get_players(true)
    
    for key, player in pairs(players) do

        local local_player = entity.get_local_player()
        local eye_x, eye_y, eye_z = client.eye_position()
        local head_x, head_y, head_z = entity.hitbox_position(player, 3)
        local entindex, damage = client.trace_bullet(local_player, eye_x, eye_y, eye_z, head_x, head_y, head_z)

        plist.set(player, 'Override Safe Point', damage > entity.get_prop(player, "m_iHealth") and 'On' or '-')
        plist.set(player, 'Override Prefer Body Aim', ui.get(dt[2]) and 'On' or '-')
    end
end)
