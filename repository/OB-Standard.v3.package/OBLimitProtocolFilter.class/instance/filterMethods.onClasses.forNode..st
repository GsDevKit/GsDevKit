filtering
filterMethods: methods onClasses: allClasses forNode: aNode
	^ methods
		select: [:m | allClasses anySatisfy: [:c | c includesSelector: m selector]]