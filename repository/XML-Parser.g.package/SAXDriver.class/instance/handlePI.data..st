handling tokens
handlePI: piTarget data: piData
	self saxHandler
		checkEOD; 
		processingInstruction: piTarget data: piData