*bootstrap-subclass-creation
subclass: aString
instVarNames: anArrayOfInstvarNames
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
options: optionsArray

"optionsArray is an Array of Symbols containing at most one of 
   #dbTransient, #instanceNonPersistent, #instancesInvariant
 and zero or more of  
   #subclassesDisallowed, #disallowGciStore, #modifiable , #traverseByCallback .
"
^ self subclass: aString
    instVarNames: anArrayOfInstvarNames
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    newVersionOf: (self class _classNamed: aString inDictionary: aDictionary)
    description: nil
    options: optionsArray