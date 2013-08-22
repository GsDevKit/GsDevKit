execution
execute

	(requestor parent isKindOf: OBAllMethodCategoryNode) 
		ifTrue: [self 
					select: (OBMethodCategoryNode 
						on: (target theClass categoryOfSelector: target selector)
						inClass: target theClass)
					with: requestor announcer ]
		ifFalse: 	[self 
					select: (OBAllMethodCategoryNode on: target theClass)
					with: requestor announcer   ].
	self select: target with: requestor announcer