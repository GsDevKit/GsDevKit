smalltalk
smalltalkAt: aGlobalName ifAbsent: aBlock 
	| glob |
	glob := self smalltalkAt: aGlobalName.
	^ glob == nil
		ifTrue: [aBlock value]
		ifFalse: [glob]