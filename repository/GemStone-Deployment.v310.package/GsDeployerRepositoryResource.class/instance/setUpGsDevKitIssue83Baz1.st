GsDevKit Issue 83
setUpGsDevKitIssue83Baz1
  "self reset"

  ^ self
    setUpPackageClassDefs:
      {#'Object'.
      #'GsDevKitIssue83A'.
      #().
      #().
      #().
      #'GsDevKitIssue83A'.
      #'GsDevKitIssue83B'.
      #().
      #().
      #().
      #'GsDevKitIssue83B'.
      #'GsDevKitIssue83C'.
      #().
      #().
      #().
      #'GsDevKitIssue83B'.
      #'GsDevKitIssue83D'.
      #().
      #().
      #()}
    methodDefs:
      {'GsDevKitIssue83B'.
      'xxx'.
      'xxx | ivar | ivar := 1. ^ivar'}
    named: 'GsDevKit83Baz-dkh.1'
    ancestors: #()