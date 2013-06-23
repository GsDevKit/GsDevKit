handling tokens
handleWhitespace: aString
	self saxHandler
		checkEOD; 
		ignorableWhitespace: aString