*core-squeak-subclass creation
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new subclass of the receiver.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history."

| desc |
self _validatePrivilege.
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass _description].
^ self subclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: desc
    isModifiable: modifyBoolean
