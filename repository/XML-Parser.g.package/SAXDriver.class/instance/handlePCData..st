handling tokens
handlePCData: aString
	self languageEnvironment
		ifNotNil: ["aString applyLanguageInfomation: self languageEnvironment" self error: 'not yet implemented'].
	self saxHandler
		checkEOD; 
		characters: aString