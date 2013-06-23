accessing
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream buffer |
	buffer := itsCollection species new: 100.
	newStream := WriteStream on: (itsCollection species new).
	[self atEnd] whileFalse: [newStream nextPutAll: (self next: buffer size into: buffer startingAt: 1)].
	^ newStream contents