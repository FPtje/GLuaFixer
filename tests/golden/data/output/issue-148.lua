local normalSounds = {Sound("honk1.wav"), Sound("honk2.wav"), Sound("honk3.wav"), Sound("honk4.wav")}
render.DrawBeam(
    self.StartPos - norm, -- Start
    self.EndPos, -- End
    64 * (1 - self.Life), -- Width
    texcoord, -- Start tex coord
    texcoord + self.Length / 128, -- End tex coord
    Color(
        -- Color (optional)
        Lerp(self.Life, 255, 50),
        Lerp(self.Life, 100, 50),
        255,
        255
    )
)

function a()
    return 1, 2, 3, 4, 5
end

-- format: multiline
local normalSounds = {
    Sound(
        "honk1.wav"
    ),
    Sound(
        "honk2.wav"
    ),
    Sound(
        "honk3.wav"
    ),
    Sound(
        "honk4.wav"
    )
}

-- format: multiline
render.DrawBeam(
    self.StartPos - norm, -- Start
    self.EndPos, -- End
    64 * (1 - self.Life), -- Width
    texcoord, -- Start tex coord
    texcoord + self.Length / 128, -- End tex coord
    Color(
        -- Color (optional)
        Lerp(
            self.Life,
            255,
            50
        ),
        Lerp(
            self.Life,
            100,
            50
        ),
        255,
        255
    )
)

-- format: multiline
function a()
    return 1, 2, 3, 4, 5
end

-- format: multiline
for a in 1, 2, 3, 4, 5 do
    print(a)
end

--[[ format: multiline ]]
local normalSounds = {
    Sound(
        "honk1.wav"
    ),
    Sound(
        "honk2.wav"
    ),
    Sound(
        "honk3.wav"
    ),
    Sound(
        "honk4.wav"
    )
}

--[[
format: multiline
]]
render.DrawBeam(
    self.StartPos - norm, -- Start
    self.EndPos, -- End
    64 * (1 - self.Life), -- Width
    texcoord, -- Start tex coord
    texcoord + self.Length / 128, -- End tex coord
    Color(
        -- Color (optional)
        Lerp(
            self.Life,
            255,
            50
        ),
        Lerp(
            self.Life,
            100,
            50
        ),
        255,
        255
    )
)

local normalSounds = {Sound("honk1.wav"), Sound("honk2.wav"), Sound("honk3.wav"), Sound("honk4.wav")}
render.DrawBeam(
    self.StartPos - norm, -- Start
    self.EndPos, -- End
    64 * (1 - self.Life), -- Width
    texcoord, -- Start tex coord
    texcoord + self.Length / 128, -- End tex coord
    Color(
        -- Color (optional)
        Lerp(self.Life, 255, 50),
        Lerp(self.Life, 100, 50),
        255,
        255
    )
)

function a()
    return 1, 2, 3, 4, 5
end

for a in 1, 2, 3, 4, 5 do
    print(a)
end