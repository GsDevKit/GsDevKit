execution
execute

	| types labels cls repos |
	types := MCRepository allConcreteSubclasses asArray.
	labels := types collect: [:ea | ea description].
	cls := OBChoiceRequest 
		prompt: 'Repository type:' 
		labels: labels
		values: types.
	cls == nil ifTrue: [ ^self ].
	repos := cls obConfigure.
	repos ~~ nil ifTrue: [ target repositoryGroup addRepository: repos ].
	self refresh