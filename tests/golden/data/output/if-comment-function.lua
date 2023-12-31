function foo()
    if true then -- This comment should not end up eating the end
        return false
    end
end

timer.Simple(60, function()
    if IsValid(ply) then -- This comment should not eat the end
        a = 1
    end
end)