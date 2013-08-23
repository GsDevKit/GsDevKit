drag and drop
dropOnClassCategory: aNode
	self theNonMetaClass category: aNode name.
	self signalSelection.
	^ true