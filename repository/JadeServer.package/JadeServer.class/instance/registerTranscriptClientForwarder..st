OmniBrowser
registerTranscriptClientForwarder: aClientForwarder

	Smalltalk at: #TranscriptProxy ifPresent: [:cl |
		cl registerTranscriptClientForwarder: aClientForwarder]