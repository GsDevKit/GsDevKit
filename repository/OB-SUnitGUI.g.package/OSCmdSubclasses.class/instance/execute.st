execution
execute

	| nodes |
	requestor selectedNode == nil ifTrue: [ ^self ].
	nodes := requestor selectedNode testHierarchy.
	requestor selectChildren: nodes.
	self update