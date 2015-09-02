drag and drop
wantsDroppedNode: aNode
	^ (super wantsDroppedNode: aNode) and: [ self isReadOnly not ]