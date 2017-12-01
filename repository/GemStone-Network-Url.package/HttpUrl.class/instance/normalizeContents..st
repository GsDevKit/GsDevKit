downloading
normalizeContents: webDocument
	(webDocument isString) ifTrue: [
		^(Smalltalk at: #MIMEDocument)
			contentType: 'text/plain'
			content: 'error occured retrieving ', self toText, ': ', webDocument
			url: (Url absoluteFromText: '')].
	webDocument contentType = (Smalltalk at: #MIMEDocument) defaultContentType ifTrue: [
		^(Smalltalk at: #MIMEDocument) contentType: ((Smalltalk at: #MIMEDocument) guessTypeFromName: self path last) 
			content: webDocument content url: webDocument url ].

	^webDocument
