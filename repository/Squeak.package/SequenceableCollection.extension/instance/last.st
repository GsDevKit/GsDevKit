*squeak-accessing
last
	"Answer the last element of the receiver.
	Raise an error if the collection is empty."

	| size |
	(size _ self size) = 0 ifTrue: [self errorEmptyCollection].
	^ self at: size