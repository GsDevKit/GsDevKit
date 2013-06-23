*core-squeak-subclass creation
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in 2.1.  The preferred methods are in the Subclass Creation
 category.  See the similar method without the constraints keyword."

"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs."

| oldClass desc |
self _validatePrivilege.
oldClass := self _nilOrClassNamed: aString .
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass _description].
^self
  indexableSubclass: aString
  instVarNames: anArrayOfStrings
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  constraints: aConstraint
  instancesInvariant: invarBoolean
  newVersionOf: oldClass
  description: desc
  isModifiable: modifyBoolean
