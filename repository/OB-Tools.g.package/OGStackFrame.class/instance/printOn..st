printing
printOn: aStream

	label == nil
		ifTrue: [aStream nextPutAll: self class name ]
		ifFalse: [aStream nextPutAll: label]