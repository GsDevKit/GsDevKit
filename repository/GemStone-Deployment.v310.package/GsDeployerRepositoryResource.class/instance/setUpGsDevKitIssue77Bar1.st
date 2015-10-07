GsDevKit Issue 77
setUpGsDevKitIssue77Bar1
  "self reset"

  ^ self
    setUpPackageClassDefs:
      {#'Object'.
      #'GsDevKitBarA'.
      #().
      #().
      #().
      #'GsDevKitBarA'.
      #'GsDevKitBarC'.
      #().
      #().
      #().
      #'Object'.
      #'GsDevKitBarB'.
      #().
      #().
      #()}
    named: 'GsDevKitBar-dkh.1'
    ancestors: #()