queries
versionsOfMethod: aSelector in: aBehavior

	| mDict coll |
	mDict := self dictionary 
		at: aBehavior 
		ifAbsent: [ 
			"Don't create an entry for non-existent Behaviors"
			(Smalltalk at: aBehavior name) ~~ nil
				ifTrue: [ self createHistoryFor: aSelector in: aBehavior ]
				ifFalse: [ ^#()]].
	^mDict 
		at: aSelector 
		ifAbsent: [ 
			"Don't create an entry for non-existent method"
			(aBehavior compiledMethodAt: aSelector ifAbsent: []) ~~ nil
				ifTrue: [
					mDict := self createHistoryFor: aSelector in: aBehavior.
					mDict at: aSelector ifAbsent: [#() ]]
				ifFalse: [ #() ]].
