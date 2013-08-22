execution
execute

	target removeChild: requestor selectedNode.
	requestor listChanged.
	self doAutoCommit