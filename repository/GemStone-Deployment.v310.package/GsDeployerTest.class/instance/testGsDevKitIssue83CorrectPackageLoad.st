tests Issue 83
testGsDevKitIssue83CorrectPackageLoad
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  "confirm that a package with a correct method still loads properly"

  | g |
  self issue83Gofer
    version: 'GsDevKit83Baz-dkh.1';
    load.
  self issue83VerifyClassHierarchy.
  g := self issue83Gofer
    version: 'GsDevKit83Baz-dkh.2';
    yourself.
  g load.
  self issue83VerifyClassHierarchy