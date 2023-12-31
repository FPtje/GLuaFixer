function a()
    return 1, 2, 3, 4
end

function a()
    return
        -- 1
        1, -- 2
        2, -- 3 -- 4
        3, -- 5
        4
    -- 6
    -- 7
end

function a()
    return
        --[[8]]
        1, --[[9]]
        2, --[[10]] --[[11]]
        3, --[[12]]
        4
    --[[13]]
    --[[14]]
end

function a()
    return
        --[[15]]
        1, --[[16]]
        2, --[[17]] --[[18]]
        function()
        print(3)
        return 4
    end, --[[19]]
        4
    --[[20]]
    --[[21]]
end

function a()
    return 1, 2, function()
        print(3)
        return 4
    end, 4
end

hook.Add("OnPrettyPrint", "test", function()
    -- yes
    a = 1 + 1
    return function()
        -- fancy
    end
    -- no
end)

for b, c, d in 2, 3, function()
    -- A function in a for loop, is it possible? Syntactically, yes!
    -- And this comment makes it _really_ awkward to pretty print!
end, 4, 5 do
    print(a, b, c, d)
end

for b, c, d in 2, 3, {"foo", "bar"}, 4, 5 do
    print(a, b, c, d)
end

for b, c, d in 2, 3, {
    a = "foo",
    b = "bar"
}, 4, 5 do
    print(a, b, c, d)
end