*core-squeak-subclass creation
variableByteSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

  ^self byteSubclass: subclassName
	classVars: (classVariableNames findTokens: ' ')  asArray
	classInstVars: #()
	poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
	inDictionary: ''
	category: category