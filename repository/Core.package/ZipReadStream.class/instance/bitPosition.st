private
bitPosition
	"Return the current bit position of the source"
	sourceStream == nil
		ifTrue:[^sourcePos * 8 + bitPos]
		ifFalse:[^sourceStream position - 1 + sourcePos * 8 + bitPos]