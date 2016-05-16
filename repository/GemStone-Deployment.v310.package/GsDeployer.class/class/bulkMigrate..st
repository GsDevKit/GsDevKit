deploying
bulkMigrate: deployNoArgBlock
  "
		1. set autoCommit to true
		2. set autoMigrate to false
		3. commit on AlmostOutOfMemoryException during evaluation of  deployNoArgBlock
		4. trap and resume Warnings after displaying description to Transcript during evaluation of  deployNoArgBlock
		5. clean up class histories and migrate all instances on one go
		6. clean up UndefinedSymbols set
		7. commit
		8. restore original autoCommit and autoMigrate.
	"

  ^ self defaultDeployerClass new
    bulkMigrate: true;
    deploy: deployNoArgBlock;
    yourself