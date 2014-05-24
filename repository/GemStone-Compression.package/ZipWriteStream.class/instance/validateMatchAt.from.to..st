deflating
validateMatchAt: pos from: startPos to: endPos
	| here |
	here := pos.
	startPos+1 to: endPos+1 do:[:i|
		(itsCollection at: i) = (itsCollection at: (here := here + 1))
			ifFalse:[^self error:'Not a match']].
	^true