*bootstrap-subclass-creation
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | cl |
		cl := self subclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			inDictionary: GsPackagePolicy current homeSymbolDict
			constraints: #[]
			instancesInvariant: false
			isModifiable: false.
		cl category: aCategoryName asString.
		cl].