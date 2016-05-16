deploying
autoMigrate: deployNoArgBlock
  "
		1. set autoCommit to true
		2. set autoMigrate to true
		3. commit on AlmostOutOfMemoryException during evaluation of  deployNoArgBlock
		4. trap and resume Warnings after displaying description to Transcript during evaluation of  deployNoArgBlock
		5. clean up class histories
		6. clean up UndefinedSymbols set
		7. commit
		8. restore original autoCommit and autoMigrate.
	"

  ^ self defaultDeployerClass new deploy: deployNoArgBlock