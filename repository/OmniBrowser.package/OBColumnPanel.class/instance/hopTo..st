navigating
hopTo: aNode 
	| column |
	column := self columns last.
	[ column
		refreshAndSignal: false;
		includesNode: aNode ] whileFalse: 
		[ column := self 
			columnBefore: column
			ifAbsent: [ ^ self jumpTo: aNode ] ].
	column select: aNode.