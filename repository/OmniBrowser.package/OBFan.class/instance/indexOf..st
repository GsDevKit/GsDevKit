selecting
indexOf: aNode 
	| index |
	index := children indexOf: aNode.
	index = 0 ifTrue: 
		[index := self children indexOf: 
			(self children 
				detect: [:ea | ea name = aNode name] 
				ifNone: [nil])].
	^index