handling tokens
handleComment: aString
	self saxHandler
		checkEOD; 
		comment: aString