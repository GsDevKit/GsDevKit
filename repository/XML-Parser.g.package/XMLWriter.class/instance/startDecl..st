writing dtd
startDecl: type
	self stream
		nextPutAll: '<!';
		nextPutAll: type asUppercase;
		space