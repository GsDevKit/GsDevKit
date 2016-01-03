tests Issue 83
issue83VerifyClassHierarchy
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | cls1Name cls2Name cls3Name cls4Name |
  cls1Name := 'GsDevKitIssue83A'.
  cls2Name := 'GsDevKitIssue83B'.
  cls3Name := 'GsDevKitIssue83C'.
  cls4Name := 'GsDevKitIssue83D'.
  self assert: (Smalltalk at: cls3Name) notNil.
  self assert: (Smalltalk at: cls4Name) notNil.
  self assert: (Smalltalk at: cls1Name) == (Smalltalk at: cls2Name) superclass.
  self assert: (Smalltalk at: cls2Name) == (Smalltalk at: cls3Name) superclass.
  self assert: (Smalltalk at: cls2Name) == (Smalltalk at: cls4Name) superclass