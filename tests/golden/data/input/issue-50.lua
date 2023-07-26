for num, ply in pairs(player.GetAll()) do --If the buddies are in the server then add them serverside
    if ply:SteamID() == v.steamid then
        -- update the name
        sql.Query("UPDATE FPP_Buddies SET name = " .. sql.SQLStr(ply:Nick()) .. " WHERE steamid = " .. sql.SQLStr(v.steamid) .. ";")
        FPP.Buddies[v.steamid].name = ply:Nick()
        RunConsoleCommand("FPP_SetBuddy", ply:UserID(), v.physgun, v.gravgun, v.toolgun, v.playeruse, v.entitydamage)
    end
end
