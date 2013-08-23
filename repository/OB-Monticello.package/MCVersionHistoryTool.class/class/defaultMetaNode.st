configuration
defaultMetaNode
	| ancestor ancestors |
	ancestor := OBMetaNode named: 'Ancestor'.
	ancestors := OBMetaNode named: 'Ancestors'.
	ancestor childAt: #withBreadthFirstAncestors put: ancestors.
	^ ancestor