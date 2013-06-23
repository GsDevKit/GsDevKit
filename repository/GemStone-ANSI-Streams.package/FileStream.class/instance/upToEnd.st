accessing
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| stream next |
	stream := WriteStream on: self collectionSpecies new.
	[ (next := self next) == nil ] whileFalse: [
			stream nextPut: next ].
	^stream contents