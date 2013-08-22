callbacks
canDrop: passenger on: node
	^node notNil and: [node wantsDroppedNode: passenger]