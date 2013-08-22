building
defaultLabel
	^ String streamContents: [ :stream |
		stream nextPutAll: super defaultLabel.
		self currentNode isNil 
			ifFalse: [ stream nextPutAll: ': '; nextPutAll: self currentNode fullName ] ]