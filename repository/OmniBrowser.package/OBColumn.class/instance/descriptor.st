printing
descriptor
	self isEmpty ifTrue: [^ 'empty'].
	self hasSelection ifTrue: [^ self selectedNode name].
	^ ''