as yet unclassified
printOn: aStream

	self generateError
		ifTrue: [ self error: 'error' ]
		ifFalse: [ self halt]