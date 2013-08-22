menu messages
referencesToIt: aSymbol

	| entry |
	entry := Smalltalk at: aSymbol ifAbsent: [nil].
	(entry isKindOf: Class) ifFalse: [entry _ entry class].
	OBReferencesBrowser browseRoot: entry asNode