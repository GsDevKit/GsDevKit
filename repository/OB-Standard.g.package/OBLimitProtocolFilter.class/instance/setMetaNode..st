filtering
setMetaNode: aMetaNode
	super setMetaNode: aMetaNode.
	aMetaNode children do: [ :each |
		(each filters includes: self)
			ifFalse: [ each addFilter: self ] ].