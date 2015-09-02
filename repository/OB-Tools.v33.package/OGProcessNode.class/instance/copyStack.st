accessing
copyStack
	| current stack count |
	current := self context.
	count := 1.
	stack := WriteStream on: String new.
	[ current notNil  ] whileTrue: [
		stack nextPutAll: '[', count asString, '] '.
		stack nextPutAll: current stackFrameString.
		current := current sender.
		count := count + 1. ].
	OBTextRequest prompt: 'Copy stack trace' template: stack contents
