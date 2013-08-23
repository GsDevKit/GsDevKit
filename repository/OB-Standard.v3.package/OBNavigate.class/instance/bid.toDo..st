as yet unclassified
bid: anInteger toDo: aBlock
	anInteger > bid ifTrue:
		[bid := anInteger.
		action := aBlock]