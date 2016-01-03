tests Issue 83
issue83VerifyClassHierarchyDoubleWhammy
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | cls4Name cls5Name cls6Name |
  self issue83VerifyClassHierarchy.
  cls4Name := 'GsDevKitIssue83D'.
  cls5Name := 'GsDevKitIssue83E'.
  cls6Name := 'GsDevKitIssue83F'.
  self assert: (Smalltalk at: cls4Name) notNil.
  self assert: (Smalltalk at: cls4Name) == (Smalltalk at: cls5Name) superclass.
  self assert: (Smalltalk at: cls4Name) == (Smalltalk at: cls6Name) superclass