as yet unclassified
nodesFrom: aCollection forNode: aNode
	| tag pragmas |
	pragmas := aNode childFilterPragmas collect: [:ea | ea argumentAt: 1 ].
	pragmas isEmpty ifTrue: [ ^#() ].
	tag := (pragmas at: self selection) asString.
	^ aCollection select: [:ea | ea tag = tag]