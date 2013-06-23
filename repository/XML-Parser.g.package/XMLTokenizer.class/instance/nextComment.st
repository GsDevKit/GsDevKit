tokenizing
nextComment
	| string |
	"Skip first -"
	self next.
	self next == $-
		ifFalse: [self errorExpected: 'second comment $-'].
	string _ self nextUpToAll: '-->'.
	self handleComment: string