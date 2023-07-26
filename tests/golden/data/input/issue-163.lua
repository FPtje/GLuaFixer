-- These should not be marked as multiline, because there is only one statement
-- in the body of each example.
timer.Create("name", 30, 0, function() if true then return end end)
timer.Create("name", 30, 0, function() return end)
timer.Create("name", 30, 0, function() end)
timer.Create("name", 30, 0)

func(function() if true then return end end)
func(function() anything() end)

-- These, on the other hand, _should_ be marked as multiline
timer.Create("name", 30, 0, function()
    -- comment here
    if true then return end
end)

timer.Create("name", 30, 0, function()
    a = 1
    return
end)
