accessing
withDefinitionDo: workBlock ifNil: nilBlock
	definition ifNil: [ ^ nilBlock value].
	^ workBlock value: definition