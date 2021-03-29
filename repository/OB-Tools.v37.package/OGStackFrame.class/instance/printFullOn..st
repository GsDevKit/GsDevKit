printing
printFullOn: aStream

	label == nil
		ifTrue: [aStream nextPutAll: self class name ]
		ifFalse: [aStream nextPutAll: label].
	aStream nextPut: Character cr.
	self tempNames withIndexDo: [ :name :index |
		aStream tab; nextPutAll: name, ': ', (self tempAt: index) printString; nextPut: Character cr].
	aStream tab; nextPutAll: 'receiver: ', self receiver printString; nextPut: Character cr.
