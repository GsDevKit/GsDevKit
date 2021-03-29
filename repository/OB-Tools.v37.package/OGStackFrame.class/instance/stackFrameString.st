printing
stackFrameString

	|str |
	str := WriteStream on: String new.
	self printFullOn: str.
	^str contents