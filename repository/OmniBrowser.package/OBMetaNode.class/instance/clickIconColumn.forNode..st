filtering
clickIconColumn: aColumn forNode: aNode
	filters do: [ :each |
		(each clickIconColumn: aColumn forNode: aNode)
			ifTrue: [ ^ self ] ]