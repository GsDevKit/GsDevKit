selecting
multipleSelect: anInteger put: aValue

	| node index |
	index := anInteger == 0
		ifTrue: [ selection ]
		ifFalse: [ anInteger ].
	node := self children at: index ifAbsent: [].
	aValue 
		ifTrue: [
			self multipleSelection at: index put: aValue.
			node ~~ nil ifTrue: [ self selectedNodes add: node ] ]
		ifFalse: [
			self multipleSelection removeKey: index ifAbsent: [].
			node ~~ nil ifTrue: [ self selectedNodes remove: node ifAbsent: [] ]].