tests Issue 83
testGsDevKitIssue83
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | cls1 cat cls2 cls1Name cls2Name |
  cat := 'GsDevKitIssue83'.
  cls1Name := 'GsDevKitIssue83A'.
  cls2Name := 'GsDevKitIssue83B'.
  cls1 := self
    createIssue83ClassNamed: cls1Name
    subclassOf: Object
    instVarNames: #().
  cls2 := self
    createIssue83ClassNamed: cls2Name
    subclassOf: cls1
    instVarNames: #().
  cls2 compileMethod: 'xxx | ivar | ivar := 1. ^ ivar' category: cat.
  self assert: (cls2 compiledMethodAt: 'xxx' ifAbsent: [  ]) notNil.
  self assert: (Smalltalk at: cls1Name) == (Smalltalk at: cls2Name) superclass.
  self
    should: [ 
      self
        createIssue83ClassNamed: cls1Name
        subclassOf: Object
        instVarNames: #(#'ivar') ]
    raise: Error.
  self assert: (Smalltalk at: cls1Name) == (Smalltalk at: cls2Name) superclass