Accessing
subject
	^subject ~~ nil
		ifTrue: [subject]
		ifFalse: ['']
