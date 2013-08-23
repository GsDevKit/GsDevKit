accessing
updateStack

	updateStack == nil ifTrue: [ updateStack := OrderedCollection new ].
	^updateStack