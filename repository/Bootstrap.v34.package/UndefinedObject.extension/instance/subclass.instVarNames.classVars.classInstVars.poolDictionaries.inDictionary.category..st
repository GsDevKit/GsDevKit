*bootstrap-subclass-creation
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [ Class homeSymbolDict ]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self subclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			options: #() ].
