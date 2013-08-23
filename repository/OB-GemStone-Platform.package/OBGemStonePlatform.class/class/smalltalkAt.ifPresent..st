smalltalk
smalltalkAt: aGlobalName ifPresent: aBlock 
	| glob |
	glob := self smalltalkAt: aGlobalName.
	^ glob ~~ nil
		ifTrue: [aBlock value: glob]
		ifFalse: [glob]