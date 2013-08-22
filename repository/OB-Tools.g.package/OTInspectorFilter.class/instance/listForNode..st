callbacks
listForNode: aNode
	^ aNode isNil
		ifTrue: [ Array new ]
		ifFalse: [ aNode filters collect: [ :each | each key ] ]