drag and drop
dropOnClass: aNode
	self methods do: [ :each | each dropOnClass: aNode ].
	aNode signalChildrenChanged.
	^ true