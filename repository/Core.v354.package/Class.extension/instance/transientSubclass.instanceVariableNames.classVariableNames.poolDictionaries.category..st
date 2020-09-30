*core-squeak-subclass creation
transientSubclass: aString
instanceVariableNames: instanceVariableNames
classVariableNames: classVariableNames
poolDictionaries: poolDictionaryNames
category: aCategoryName

| res |
res :=  self subclass: aString
    instVarNames: (instanceVariableNames findTokens: ' ') asArray
    classVars: (classVariableNames findTokens: ' ')  asArray
    classInstVars: #()
    poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
    inDictionary: ''
    category: aCategoryName
    options: #( #dbTransient )