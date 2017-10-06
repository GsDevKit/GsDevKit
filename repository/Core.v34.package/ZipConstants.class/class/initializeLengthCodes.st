pool initialization
initializeLengthCodes
	| length |
	BaseLength _ WordArray new: MaxLengthCodes.
	MatchLengthCodes _ WordArray new: MaxMatch - MinMatch + 1.
	length _ 0.
	1 to: MaxLengthCodes - 1 do:[:code|
		BaseLength at: code put: length.
		1 to: (1 bitShift: (ExtraLengthBits at: code)) do:[:n|
			length _ length + 1.
			MatchLengthCodes at: length put: NumLiterals + code]].