execution
execute
	| catName organization |
	catName := self requestCategoryToUser.
	organization := target organization.
	organization isClassOrganizer
		ifTrue: [ organization := target theClass ].
	catName ifNotNil: [self doAutoAbort.
					organization addCategory: catName.
						self 
							select: (target categoryNodeNamed: catName) 
							with: requestor announcer.
						self doAutoCommit].