accessing
label
	^ requestor browser theClass = Object 
		ifTrue: ['limit']
		ifFalse: ['limit: ' , self limitClass name]