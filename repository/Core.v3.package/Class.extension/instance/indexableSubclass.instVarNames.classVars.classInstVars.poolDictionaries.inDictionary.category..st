*core-squeak-subclass creation
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^self indexableSubclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		options: #()