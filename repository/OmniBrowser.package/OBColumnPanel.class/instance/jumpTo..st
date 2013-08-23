navigating
jumpTo: aNode 
	| column |
	self selectAncestorsOf: aNode.
	column := self columns reversed 
				detect: [:ea | ea selectedNode = aNode] 
				ifNone: [^ self].
	self clearAfter: column next.
	self announcer announce: (OBSelectionChanged column: column)