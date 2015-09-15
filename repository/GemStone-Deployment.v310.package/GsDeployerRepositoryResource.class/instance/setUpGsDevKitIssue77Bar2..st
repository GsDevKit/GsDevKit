GsDevKit Issue 77
setUpGsDevKitIssue77Bar2: ancestors
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
    named: 'GsDevKitBar-dkh.2'
    ancestors: ancestors