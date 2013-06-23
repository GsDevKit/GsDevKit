handling tokens
handleXMLDecl: attributes namespaces: namespaces
	self saxHandler
		checkEOD; 
		documentAttributes: attributes.
	self usesNamespaces
		ifTrue: [
			namespaces keysAndValuesDo: [:ns :uri |
				self scope declareNamespace: ns uri: uri]]