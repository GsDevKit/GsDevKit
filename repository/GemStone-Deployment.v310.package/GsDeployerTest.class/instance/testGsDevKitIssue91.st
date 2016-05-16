tests
testGsDevKitIssue91
  "https://github.com/GsDevKit/GsDevKit/issues/91"

  "No migration whatsoever"

  | instances deployerClass |
  System commit.
  deployerClass := GsDeployer defaultDeployerClass.
  instances := GsDeployer
    noMigrationDuring: [ self doDeployForAutoMigrationTest ].
  instances
    do: [ :instance | 
      "unmigrated instances should not understand the new message"
      self
        should: [ (instance perform: #'e') == nil ]
        raise: MessageNotUnderstood ].
  #(#'GsDeployerBarA' #'GsDeployerBarC')
    do: [ :className | 
      | obj |
      "new instances should understand the new message"
      obj := (Smalltalk at: className) new.
      self assert: (obj perform: #'e') == nil ].
  System abort.
  self assert: GsDeployer defaultDeployerClass == deployerClass