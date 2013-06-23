initialization
initialize
	"SmalltalkProxy initialize"
	
	| userGlobals |
	userGlobals := System myUserProfile symbolList objectNamed: #UserGlobals.
	userGlobals at: #Smalltalk put: self new.
	self _resolveUndeclaredSymbolsFor: #Smalltalk