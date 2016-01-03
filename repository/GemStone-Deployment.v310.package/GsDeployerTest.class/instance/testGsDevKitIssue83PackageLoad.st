tests Issue 83
testGsDevKitIssue83PackageLoad
  "https://github.com/GsDevKit/GsDevKit/issues/83"

  | g |
  self issue83Gofer
    version: 'GsDevKit83Foo-dkh.1';
    load.
  self issue83Gofer
    version: 'GsDevKit83Bar-dkh.1';
    load.
  self issue83VerifyClassHierarchy.
  g := self issue83Gofer
    version: 'GsDevKit83Foo-dkh.2';
    yourself.
  self
    should: [ 
      [ g load ]
        on: Warning
        do: [ :ex | ex resume ] ]
    raise: Error.
  self issue83VerifyClassHierarchy