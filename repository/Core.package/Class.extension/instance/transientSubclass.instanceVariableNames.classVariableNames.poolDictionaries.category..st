*core-squeak-subclass creation
transientSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self transientSubclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
