*core-squeak-subclass creation
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName
inDictionary: aDictionary
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

| cl |
cl := self 
	byteSubclass: aString
	classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars
	poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary
	instancesInvariant: invarBoolean
    isModifiable: modifyBoolean.
cl category: aCategoryName asString.
^cl