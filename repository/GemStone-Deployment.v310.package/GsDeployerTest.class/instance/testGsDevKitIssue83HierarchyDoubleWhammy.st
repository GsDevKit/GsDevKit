tests Issue 83
testGsDevKitIssue83HierarchyDoubleWhammy
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | cls1 cat cls2 cls3 cls4 cls5 cls6 cls1Name cls2Name cls3Name cls4Name cls5Name cls6Name |
  cat := 'GsDevKitIssue83'.
  cls1Name := 'GsDevKitIssue83A'.
  cls2Name := 'GsDevKitIssue83B'.
  cls3Name := 'GsDevKitIssue83C'.
  cls4Name := 'GsDevKitIssue83D'.
  cls5Name := 'GsDevKitIssue83E'.
  cls6Name := 'GsDevKitIssue83F'.
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
  cls5 := self
    createIssue83ClassNamed: cls5Name
    subclassOf: cls4
    instVarNames: #().
  cls5 compileMethod: 'xxx | ivar | ivar := 1. ^ ivar' category: cat.
  self assert: (cls5 compiledMethodAt: 'xxx' ifAbsent: [  ]) notNil.
  cls6 := self
    createIssue83ClassNamed: cls6Name
    subclassOf: cls4
    instVarNames: #().
  self issue83VerifyClassHierarchyDoubleWhammy.
  self
    should: [ 
      self
        createIssue83ClassNamed: cls1Name
        subclassOf: Object
        instVarNames: #(#'ivar') ]
    raise: Error.
  self issue83VerifyClassHierarchyDoubleWhammy