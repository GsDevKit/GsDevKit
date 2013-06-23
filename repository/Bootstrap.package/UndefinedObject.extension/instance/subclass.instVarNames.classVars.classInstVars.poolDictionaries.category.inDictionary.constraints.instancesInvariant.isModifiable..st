*bootstrap
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

| cl |
cl := self subclass: aString
	instVarNames: anArrayOfStrings
	classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars
	poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary
	constraints: aConstraint
	instancesInvariant: invarBoolean
	isModifiable: modifyBoolean.
cl category: aCategoryName asString.
^cl
