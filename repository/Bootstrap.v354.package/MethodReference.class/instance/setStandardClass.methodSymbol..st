setting
setStandardClass: aClass methodSymbol: methodSym

	aClass isMeta
		ifTrue: [ classSymbol := aClass instanceSymbol]
		ifFalse: [classSymbol := aClass name].
	classIsMeta := aClass isMeta.
	methodSymbol := methodSym.
	stringVersion := aClass name , ' ' , methodSym.