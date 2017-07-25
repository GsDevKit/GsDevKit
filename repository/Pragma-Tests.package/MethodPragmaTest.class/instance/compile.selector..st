utilities
compile: aString selector: aSelector
  ^ self class
    compileMethod: (aSelector asString , ' ', aString)
    dictionaries: GsSession currentSession symbolList 
    category: self methodCategory
    environmentId: 0
