initializing
setName: aSymbol
	name := (aSymbol copyUpTo: $ ) asSymbol.
	isMeta := aSymbol endsWith: ' class'.