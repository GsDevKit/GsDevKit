*core-squeak-subclass creation
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
instancesInvariant: invarBoolean

"Creates and returns a new byte subclass of the receiver.  You are not
 permitted to modify the new class after it is created.  If the receiver is not
 some kind of String class, then instances of the new class store and return
 SmallIntegers in the range 0 - 255.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format, if they are NSCs, or if they have instance variables."

self _validatePrivilege.
^ self byteSubclass: aString
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    newVersionOf: oldClass
    description: (oldClass isNil ifTrue: [nil] ifFalse: [oldClass _description])
    isInvariant: invarBoolean
