*core-squeak-subclass creation
variableSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self indexableSubclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
