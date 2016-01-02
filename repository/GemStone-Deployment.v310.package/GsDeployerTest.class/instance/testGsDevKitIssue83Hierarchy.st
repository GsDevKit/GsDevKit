tests Issue 83
testGsDevKitIssue83Hierarchy
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | cls1 cat cls2 cls3 cls4 cls1Name cls2Name cls3Name cls4Name |
  cat := 'GsDevKitIssue83'.
  cls1Name := 'GsDevKitIssue83A'.
  cls2Name := 'GsDevKitIssue83B'.
  cls3Name := 'GsDevKitIssue83C'.
  cls4Name := 'GsDevKitIssue83D'.
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
  cls3 := self
    createIssue83ClassNamed: cls3Name
    subclassOf: cls2
    instVarNames: #().
  cls4 := self
    createIssue83ClassNamed: cls4Name
    subclassOf: cls2
    instVarNames: #().
  self assert: (Smalltalk at: cls3Name) notNil.
  self assert: (Smalltalk at: cls4Name) notNil.
  self assert: (Smalltalk at: cls1Name) == (Smalltalk at: cls2Name) superclass.
  self assert: (Smalltalk at: cls2Name) == (Smalltalk at: cls3Name) superclass.
  self assert: (Smalltalk at: cls2Name) == (Smalltalk at: cls4Name) superclass.
  self
    should: [ 
      self
        createIssue83ClassNamed: cls1Name
        subclassOf: Object
        instVarNames: #(#'ivar') ]
    raise: Error.
  self assert: (Smalltalk at: cls3Name) notNil.
  self assert: (Smalltalk at: cls4Name) notNil.
  self assert: (Smalltalk at: cls1Name) == (Smalltalk at: cls2Name) superclass.
  self assert: (Smalltalk at: cls2Name) == (Smalltalk at: cls3Name) superclass.
  self assert: (Smalltalk at: cls2Name) == (Smalltalk at: cls4Name) superclass