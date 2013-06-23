accessing
removeKey: aGlobalName ifAbsent: aBlock

	^(System myUserProfile symbolList objectNamed: #UserGlobals)
		removeKey: aGlobalName 
		ifAbsent: aBlock