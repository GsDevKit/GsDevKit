selecting
selectChildren: nodes

	| children |
	children := self children.
	nodes do: [:node | | index |
		index := children indexOf: node. 
		self multipleSelect: index put: true ].
	self changed: #multipleSelection