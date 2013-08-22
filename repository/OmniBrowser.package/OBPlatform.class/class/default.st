as yet unclassified
default
	| classes |
	classes _ self subclasses.
	^ classes isEmpty
		ifTrue: [self error: 'No platform available']
		ifFalse: [classes anyOne default]