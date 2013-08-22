instance creation
fromMethodNode: aMethodNode
	^ self new 
		setMessage: aMethodNode selector
		selector: aMethodNode selector
		class: aMethodNode theClass