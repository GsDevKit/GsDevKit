GsDevKit Issue 77
setUpGsDevKitIssue77Bar3: ancestors
  "self reset"

  ^ self
    setUpPackageClassDefs:
      {#'GsDevKitBarB'.
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
    named: 'GsDevKitBar-dkh.3'
    ancestors: ancestors