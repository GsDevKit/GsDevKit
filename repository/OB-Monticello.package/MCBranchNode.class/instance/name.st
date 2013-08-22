as yet unclassified
name
	^ branch isEmptyOrNil
		ifTrue: ['--trunk--']
		ifFalse: [branch]