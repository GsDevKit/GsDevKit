private
moveContentsToFront
	"Need to update crc here"
	| delta |
	self updateCrc.
	delta := (blockPosition - WindowSize).
	delta <= 0 ifTrue:[^self].
	"Move collection"
	itsCollection 
		squeakReplaceFrom: 1 
		to: itsCollection size - delta 
		with: itsCollection 
		startingAt: delta+1.
	position := position - delta.
	"Move hash table entries"
	blockPosition := blockPosition - delta.
	blockStart := blockStart - delta.
	self updateHashTable: hashHead delta: delta.
	self updateHashTable: hashTail delta: delta.
	crcPosition := position.