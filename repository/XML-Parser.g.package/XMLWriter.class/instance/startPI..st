private tags
startPI: identifier
	self stream
		nextPutAll: '<?';
		nextPutAll: identifier;
		space