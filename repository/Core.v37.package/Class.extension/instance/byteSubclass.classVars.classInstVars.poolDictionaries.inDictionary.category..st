*core-squeak-subclass creation
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^self 
		byteSubclass: aString
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
      	category: aCategoryName
		options: #()