updating
reclaimPanes
	| old |
	old := columns size.
	[self okToReclaimPane] whileTrue: [self popColumn].
	columns size = old ifFalse: [self changed: #columns]