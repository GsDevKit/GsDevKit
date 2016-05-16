deploying
noMigrationDuring: aBlock
  "Use the GsNonmigratingDeployer for duration of aBlock"

  | oldBinding |
  oldBinding := DefaultClassBinding.
  [ 
  self defaultDeployerClass: GsNonmigratingDeployer.
  ^ aBlock value ]
    ensure: [ 
      DefaultClassBinding := oldBinding.
      System commitTransaction
        ifFalse: [ self error: 'Commit failed!' ] ]