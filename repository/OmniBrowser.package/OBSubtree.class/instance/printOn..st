printing
printOn: aStream
	aStream
		nextPutAll: 'Subtree< '.
	self state readStream do: [ :e | e printOn: aStream. aStream nextPut: Character space].
	aStream
		nextPut: $>