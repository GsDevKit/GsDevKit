configuration
defaultMetaNode
	| method root |
	method := OBMetaNode named: 'Method'.
	root := OBMetaNode named: 'Root'.
	root
		childAt: #children put: method.
	method
		displaySelector: #fullName;
		ancestrySelector: #isDescendantOfMethod:;
		childAt: #overrides put: method.
	^ root