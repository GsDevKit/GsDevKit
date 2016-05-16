tests
testMonticelloAutoMigrate
  | instances |
  instances := self doDeployForAutoMigrationTest.
  instances
    do: [ :instance | 
      self
        assert:
          (instance perform: instance sampleValue) == (instance perform: #'sampleValue').
      self assert: (instance perform: #'e') == nil ]