running
tearDown
  | aGofer |
  super tearDown.
  aGofer := (Smalltalk at: #'Gofer') new.
  self tearDownPackages: aGofer.
  aGofer references notEmpty
    ifTrue: [ aGofer unload ].
  Smalltalk removeKey: #'GsDeployer_Test_Repository' ifAbsent: [  ].
  self tearDownClasses