*squeak
templateForSubclassOf: priorClassName category: systemCategoryName 
	"Answer an expression that can be edited and evaluated in order to define a new class, given that the class previously looked at was as given"

	| aClass type anArray dictionaryName |
	aClass := System myUserProfile symbolList objectNamed: priorClassName asString asSymbol.
	anArray := System myUserProfile dictionaryAndSymbolOf: aClass.
	dictionaryName :=  (anArray == nil or: [ (anArray at: 1) == GsPackagePolicy current homeSymbolDict])
		ifTrue: [ '' ]
		ifFalse: [
			anArray := System myUserProfile dictionaryAndSymbolOf: (anArray at: 1).
			(anArray == nil or: [(anArray at: 2) == #Globals])
				ifTrue: [ '' ]
				ifFalse: [ (anArray at: 2) asString ]].
	type := aClass typeOfClass.
	^type = #bytes
		ifTrue: [
			priorClassName asString, (self subclassTypeFor: aClass), '''NameOfSubclass''
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ', dictionaryName printString, '
	category: ''', systemCategoryName asString, ''''  ]
		ifFalse: [
			priorClassName asString, (self subclassTypeFor: aClass), '''NameOfSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ', dictionaryName printString, '
	category: ''', systemCategoryName asString, '''' ]