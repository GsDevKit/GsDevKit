printing
printOn: aStream
	aStream nextPut: $<.
	self keyword precedence = 1
		ifTrue: [ aStream space; nextPutAll: self keyword ]
		ifFalse: [
			self keyword keywords with: self arguments do: [ :key :arg |
				aStream space; nextPutAll: key; space; print: arg ]].
	aStream space; nextPut: $>.