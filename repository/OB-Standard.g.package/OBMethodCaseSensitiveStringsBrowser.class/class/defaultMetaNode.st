as yet unclassified
defaultMetaNode
	| matches selection |
	selection := OBMetaNode named: 'Selection'.
	matches := OBMetaNode named: 'Methods'.
	selection childAt: #caseSensitiveMethods put: matches.
	matches
		displaySelector: #fullName;
		ancestrySelector: #isAncestorOf:.
	^ selection