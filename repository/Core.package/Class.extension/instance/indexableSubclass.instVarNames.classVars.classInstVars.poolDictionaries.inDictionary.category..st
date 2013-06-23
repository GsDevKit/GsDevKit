*core-squeak-subclass creation
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self indexableSubclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			instancesInvariant: false
			isModifiable: false ]