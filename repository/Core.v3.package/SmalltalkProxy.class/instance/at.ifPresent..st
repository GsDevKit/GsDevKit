accessing
at: aGlobalName ifPresent: aBlock

	| glob |
	aGlobalName isNil ifTrue:[^nil].
	glob := System myUserProfile symbolList objectNamed: aGlobalName.
	^glob ~~ nil
		ifTrue: [aBlock value: glob]
		ifFalse: [glob]