*core-squeak-subclass creation
subclass: subclassName
instanceVariableNames: instanceVariableNames
classVariableNames: classVariableNames
poolDictionaries: poolDictionaryNames
category: category

	^self subclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
		options: #()
