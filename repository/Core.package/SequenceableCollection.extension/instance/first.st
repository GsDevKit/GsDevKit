*core-squeak
first
	"Answer the first element of the receiver.
	Raise an error if the collection is empty."

	self size = 0 ifTrue: [self errorEmptyCollection].
	^ self at: 1