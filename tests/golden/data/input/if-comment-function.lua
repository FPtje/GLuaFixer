function foo()
    if true then -- This comment should not end up eating the end
        return false
    end
end

timer.Simple(60, function() if IsValid(ply) then a = 1 end end) -- This comment should not eat the end
