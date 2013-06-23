deflating
compare: here with: matchPos min: minLength
	"Compare the two strings and return the length of matching characters.
	minLength is a lower bound for match lengths that will be accepted.
	Note: here and matchPos are zero based."
	| length |
	"First test if we can actually get longer than minLength"
	(itsCollection at: here+minLength+1) = (itsCollection at: matchPos+minLength+1)
		ifFalse:[^0].
	(itsCollection at: here+minLength) = (itsCollection at: matchPos+minLength)
		ifFalse:[^0].
	"Then test if we have an initial match at all"
	(itsCollection at: here+1) = (itsCollection at: matchPos+1)
		ifFalse:[^0].
	(itsCollection at: here+2) = (itsCollection at: matchPos+2)
		ifFalse:[^1].
	"Finally do the real comparison"
	length := 3.
	[length <= MaxMatch and:[
		(itsCollection at: here+length) = (itsCollection at: matchPos+length)]]
			whileTrue:[length := length + 1].
	^length - 1