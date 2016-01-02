tests
testGsDevKitIssue83
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | cls1 cat cls2 cls2Name |
  cat := 'GsDevKitIssue83'.
  cls2Name := 'GsDevKitIssue83B'.
  cls1 := Object
    subclass: 'GsDevKitIssue83A'
    instVarNames: #()
    classVars: #()
    classInstVars: #()
    poolDictionaries: #()
    inDictionary: ''
    category: cat
    options: #().
  cls2 := cls1
    subclass: cls2Name
    instVarNames: #()
    classVars: #()
    classInstVars: #()
    poolDictionaries: #()
    inDictionary: ''
    category: cat
    options: #().
  cls2 compileMethod: 'xxx | ivar | ivar := 1. ^ ivar' category: cat.
  self assert: (cls2 compiledMethodAt: 'xxx' ifAbsent: [  ]) notNil.
  self
    should: [ 
      Object
        subclass: 'GsDevKitIssue83A'
        instVarNames: #(#'ivar')
        classVars: #()
        classInstVars: #()
        poolDictionaries: #()
        inDictionary: ''
        category: cat
        options: #() ]
    raise: Error