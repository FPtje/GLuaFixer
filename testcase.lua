local normalSounds = {
	Sound("honk1.wav"),
	Sound("honk2.wav"), -- foo
	Sound("honk3.wav"),
	Sound("honk4.wav")
}
render.DrawBeam{
	self.StartPos - norm, -- Start
	self.EndPos, -- End
	64 * (1 - self.Life), -- Width
	texcoord, -- Start tex coord
	texcoord + self.Length / 128, -- End tex coord
	Color( -- Color (optional)
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
}
