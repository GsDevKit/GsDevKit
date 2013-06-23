private
sendCrlfString: aString
	^(self write: aString, self crlfString) == (aString size + 2)
