*core-squeak-subclass creation
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs."

self _validatePrivilege.
^ self indexableSubclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: (oldClass isNil ifTrue: [nil] ifFalse: [oldClass _description])
    isModifiable: modifyBoolean
