printing
asString

	|str |
	str := WriteStream on: String new.
	self printOn: str.
	^str contents