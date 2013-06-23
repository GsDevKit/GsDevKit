handling tokens
handleCData: aString
	self saxHandler
		checkEOD; 
		characters: aString