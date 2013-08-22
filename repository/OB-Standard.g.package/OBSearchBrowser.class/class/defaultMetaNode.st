as yet unclassified
defaultMetaNode
	| root search |
	root := OBMetaNode named: 'root'.
	search := OBMetaNode named: 'search'.
	root childAt: #children put: search.
	self buildMetagraphOn: search.
	^root