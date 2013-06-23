accessing
at: aGlobalName put: anObject

	^(System myUserProfile symbolList objectNamed: #UserGlobals)
		at: aGlobalName
		put: anObject