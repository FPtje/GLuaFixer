local normalSounds = {Sound("honk1.wav"), Sound("honk2.wav"), Sound("honk3.wav"), Sound("honk4.wav")}
render.DrawBeam(self.StartPos - norm, self.EndPos, 64 * (1 - self.Life), texcoord, texcoord + self.Length / 128, Color(Lerp(self.Life, 255, 50), Lerp(self.Life, 100, 50), 255, 255)) -- Start -- End -- Width -- Start tex coord -- End tex coord -- Color (optional)
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
    self.StartPos - norm,
    self.EndPos,
    64 * (1 - self.Life),
    texcoord,
    texcoord + self.Length / 128,
    Color(
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

-- Start
-- End
-- Width
-- Start tex coord
-- End tex coord
-- Color (optional)
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
    self.StartPos - norm,
    self.EndPos,
    64 * (1 - self.Life),
    texcoord,
    texcoord + self.Length / 128,
    Color(
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

-- Start
-- End
-- Width
-- Start tex coord
-- End tex coord
-- Color (optional)
local normalSounds = {Sound("honk1.wav"), Sound("honk2.wav"), Sound("honk3.wav"), Sound("honk4.wav")}
render.DrawBeam(self.StartPos - norm, self.EndPos, 64 * (1 - self.Life), texcoord, texcoord + self.Length / 128, Color(Lerp(self.Life, 255, 50), Lerp(self.Life, 100, 50), 255, 255)) -- Start -- End -- Width -- Start tex coord -- End tex coord -- Color (optional)
function a()
    return 1, 2, 3, 4, 5
end

for a in 1, 2, 3, 4, 5 do
    print(a)
end