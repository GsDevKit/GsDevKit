configuration
defaultMetaNode
	| nodes |
	nodes := Dictionary new.
	self
		initializeNodesIn: nodes;
		connectNodesIn: nodes.
	self chasingBrowsers ifTrue: [self addChasingFiltersTo: nodes].
	^ nodes at: #class