accessing
at: aGlobalName ifAbsent: aBlock

	| glob |
	aGlobalName isNil ifTrue:[^aBlock value].
	glob := System myUserProfile symbolList objectNamed: aGlobalName.
	^glob == nil
		ifTrue: [aBlock value]
		ifFalse: [glob]