updating
noteChanges
	hasChanges ifFalse: [self changed: #noteChanges ].
	hasChanges := true