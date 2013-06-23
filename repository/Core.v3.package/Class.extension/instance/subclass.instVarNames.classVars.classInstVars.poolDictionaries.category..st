*core-squeak-subclass creation
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self subclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
	      options: #() 
