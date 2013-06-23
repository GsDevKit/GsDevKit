writing dtd
startDecl: type named: aString
	self stream
		nextPutAll: '<!';
		nextPutAll: type asUppercase;
		space;
		nextPutAll: aString;
		space