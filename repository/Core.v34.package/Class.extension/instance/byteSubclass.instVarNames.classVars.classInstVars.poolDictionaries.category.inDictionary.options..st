*core-squeak-subclass creation
byteSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName
inDictionary: aDictionary
options: optionsArray
| cl |
cl := self byteSubclass: aString
  instVarNames: anArrayOfStrings
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  options: optionsArray .
cl category: aCategoryName asString.
^cl
