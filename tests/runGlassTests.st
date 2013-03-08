| names |
Transcript cr; show: 'travis--->runGlassTests.st'.
names := Array new.
ConfigurationOfGLASS project currentVersion spec
  projectDo: [ :projectSpec | names add: projectSpec projectReference className asString ]
  packageDo: [ :ignored |  ]
  groupDo: [ :ignored |  ].
TravisCIHarness
  value: names
  value: 'TravisCISuccess.txt' 
  value: 'TravisCIFailure.txt'.

