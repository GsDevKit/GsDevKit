GsDevKit Issue 83
setUpGsDevKitIssue83Bar1
  "self reset"

  ^ self
    setUpPackageClassDefs:
      {#'GsDevKitIssue83A'.
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
    named: 'GsDevKit83Bar-dkh.1'
    ancestors: #()