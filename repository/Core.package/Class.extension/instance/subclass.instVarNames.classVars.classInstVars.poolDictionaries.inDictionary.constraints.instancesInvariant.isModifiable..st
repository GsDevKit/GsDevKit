*core-squeak-subclass creation
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in 2.1  The perferred methods are in the category Subclass Creation;
 see the similar method without the constraints keyword."

"Creates and returns a new subclass of the receiver.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history."

| oldClass desc |
self _validatePrivilege.
oldClass := self _nilOrClassNamed: aString .
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass _description].
^self
  subclass: aString
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
