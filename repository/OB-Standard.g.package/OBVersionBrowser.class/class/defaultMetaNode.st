configuration
defaultMetaNode
	| version |
	version := OBMetaNode named: 'Version'.
	version ancestrySelector: #isDescendantOfMethodVersion:.
	^ (OBMetaNode named: 'Method') 
		childAt: #versions put: version; yourself.