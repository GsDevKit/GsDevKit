instance creation
on: aSelector inMethodNode: aNode
	^ self 
		on: aSelector 
		inMethod: aNode selector 
		inClass: aNode theClass