actions
pickRepositorySatisfying: aBlock
	| list |
	list := MCRepositoryGroup default repositories select: aBlock.
	^OBChoiceRequest 
		prompt: 'Repository:' 
		labels: (list collect: [:ea | ea description])
		values: list.