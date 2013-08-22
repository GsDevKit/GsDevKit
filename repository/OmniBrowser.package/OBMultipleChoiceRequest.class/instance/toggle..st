accessing
toggle: anObject
	(selection includes: anObject)
		ifTrue: [ selection remove: anObject ]
		ifFalse: [ selection add: anObject ]