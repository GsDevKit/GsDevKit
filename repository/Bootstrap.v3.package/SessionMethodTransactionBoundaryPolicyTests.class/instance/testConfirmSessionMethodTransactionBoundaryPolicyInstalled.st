tests
testConfirmSessionMethodTransactionBoundaryPolicyInstalled
	"after reports from a couple of customers, that TransactionBoundaryDefaultPolicy 
		current WAS NOT an instance of SessionMethodTransactionBoundaryPolicy as 
		expected (HR10153), it makes sense to add this test to ensure that 
		SessionMethodTransactionBoundaryPolicy is installed properly ...  the main 
		concern being that somehow the installed transaction boundary policy is being 
		disturbed during the upgrade process, so running this test post upgrade would 
		serve as an early warning for upgrade troubles"

	"also see https://github.com/GsDevKit/GsDevKit/issues/129"

	"if this test fails, the rememdy is `SessionMethodTransactionBoundaryPolicy install`"

	self
		assert:
			(TransactionBoundaryDefaultPolicy current
				isKindOf: SessionMethodTransactionBoundaryPolicy)