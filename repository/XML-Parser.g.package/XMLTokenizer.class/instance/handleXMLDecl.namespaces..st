handling tokens
handleXMLDecl: attributes namespaces: namespaces
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]