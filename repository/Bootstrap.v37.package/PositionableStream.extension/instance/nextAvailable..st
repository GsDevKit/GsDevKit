*bootstrap
nextAvailable: count
	"Returns up to the next count elements in the receiver's collection."

	^self 
		next: count 
		into: itsCollection species new 
		startingAt: 1