*core-squeak-subclass creation
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName
options: optionsArray

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [ Class homeSymbolDict ]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self indexableSubclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			options: optionsArray
		]
