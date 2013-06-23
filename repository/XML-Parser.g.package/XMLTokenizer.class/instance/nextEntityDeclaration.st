tokenizing dtd
nextEntityDeclaration
	| entityName entityDef referenceClass reference |
	self skipSeparators.
	referenceClass _ self peek == $%
		ifTrue: [
			self next.
			self skipSeparators.
			DTDParameterEntityDeclaration]
		ifFalse: [DTDEntityDeclaration].
	entityName _ self nextLiteral.
	self skipSeparators.
	entityDef _ (self peek == $" or: [self peek == $'])
		ifTrue: [self nextEntityValue]
		ifFalse: [self nextExternalId].
	self skipUpTo: $>.
	reference _ referenceClass name: entityName value: entityDef.
	reference registerIn: self.
	^reference