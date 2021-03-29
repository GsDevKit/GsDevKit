operations
assureExistence

	self exists
		ifFalse: [ self containingDirectory assureExistenceOfPath: self localName ].