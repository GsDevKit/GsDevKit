drag and drop
acceptDroppedNode: aNode
	(self value perform: self selector withArguments: (Array with: aNode value))
		inspect