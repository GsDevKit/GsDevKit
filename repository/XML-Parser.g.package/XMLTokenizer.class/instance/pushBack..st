streaming
pushBack: aString
	| pushBackString |
	pushBackString _ peekChar
		ifNil: [aString]
		ifNotNil: [peekChar asString , aString].
	peekChar _ nil.
	self pushStream: (ReadStream on: pushBackString)