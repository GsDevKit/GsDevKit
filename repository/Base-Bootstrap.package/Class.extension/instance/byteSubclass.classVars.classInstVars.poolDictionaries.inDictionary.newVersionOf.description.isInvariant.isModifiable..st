*base-bootstrap
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
description: aDescription
isInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in GemStone 4.1.  The preferred methods are in the Subclass Creation
 category.  Look for the method that omits this method's keyword description:
 and changes its keyword isInvariant: to instancesInvariant:."

| hist theFormat |
self _validatePrivilege.
oldClass ~~ nil ifTrue:[  "fix bug 11833"
  oldClass _validateClass: Class .
  hist := oldClass classHistory .
  ].

(self subclassesDisallowed) ifTrue: [
  ^ self _error: #classErrSubclassDisallowed].
aDictionary _validateClass: SymbolDictionary.
(instVars ~~ 0) ifTrue: [^ self _error: #classErrByteObjInstVars].
((self instancesInvariant) & (invarBoolean not)) ifTrue: [
  ^ self _error: #classErrInvariantSuperClass
  ].
(self isNsc) ifTrue: [ ^ aString _error: #classErrBadFormat ].
theFormat := (format bitAnd: 16r3 bitInvert) bitOr: (16r1 + 16r4) . 
invarBoolean ifTrue:[ theFormat := theFormat bitOr: 16r8 ].

^ self _subclass: aString
         instVarNames:  #() 
         format: theFormat
         constraints:  #() 
         classVars: anArrayOfClassVars
         classInstVars: anArrayOfClassInstVars
         poolDictionaries: anArrayOfPoolDicts
         inDictionary: aDictionary
         inClassHistory: hist
         description: aDescription
         isModifiable: modifyBoolean