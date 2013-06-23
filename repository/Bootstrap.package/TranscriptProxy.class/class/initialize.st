initialization
initialize
	"TranscriptProxy initialize"
	
	| userGlobals |
	userGlobals := System myUserProfile symbolList objectNamed: #UserGlobals.
	userGlobals at: #Transcript put: self.
	self _resolveUndeclaredSymbolsFor: #Transcript