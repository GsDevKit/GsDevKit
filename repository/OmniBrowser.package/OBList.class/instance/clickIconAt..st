nodes
clickIconAt: anInteger
	| node |
	node := self fan nodeAt: anInteger.
	node isNil ifFalse: [ node metaNode clickIconColumn: column forNode: node ]