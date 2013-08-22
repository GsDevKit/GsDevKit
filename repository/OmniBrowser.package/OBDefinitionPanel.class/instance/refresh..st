updating
refresh: announcement
	| oldDefinition | 
	definition ifNil: [^ self].
	oldDefinition := definition.
	definition := self getDefinition ifNil: [oldDefinition].
	definition text = oldDefinition text ifTrue: [^ self].
	self canDiscardEdits
			ifTrue: [self changed: #text]
			ifFalse: [self changed: #codeChangedElsewhere]