*core-squeak-subclass creation
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName
options: optionsArray

^Class _defineClassNamed: aString with: [ | symbolDict |
    symbolDict := aDictionaryName isEmpty
	    ifTrue: [GsPackagePolicy current homeSymbolDict]
	    ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].

    self byteSubclass: aString
	    classVars: anArrayOfClassVars
	    classInstVars: anArrayOfClassInstVars
	    poolDictionaries: anArrayOfPoolDicts
	    category: aCategoryName
	    inDictionary: symbolDict
	    options: optionsArray 
  ].
