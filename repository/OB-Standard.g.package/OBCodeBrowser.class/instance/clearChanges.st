updating
clearChanges

	hasChanges ifTrue: [self changed: #clearChanges ].
	hasChanges := false