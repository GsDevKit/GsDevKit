writing dtd
startDeclaration: dtdName
	self startDecl: 'DOCTYPE' named: dtdName.
	self stream
		nextPut: $[;
		cr