tests Issue 83
createIssue83ClassNamed: className subclassOf: aClass instVarNames: instVarNames
  ^ aClass
    subclass: className
    instVarNames: instVarNames
    classVars: #()
    classInstVars: #()
    poolDictionaries: #()
    inDictionary: ''
    category: self issue83ClassCategory
    options: #()