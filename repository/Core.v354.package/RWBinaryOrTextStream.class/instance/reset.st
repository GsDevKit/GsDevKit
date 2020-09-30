Positioning
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary := false].
	itsCollection class == ByteArray ifTrue: ["Store as String and convert as needed."
		itsCollection := itsCollection asString.
		isBinary := true].