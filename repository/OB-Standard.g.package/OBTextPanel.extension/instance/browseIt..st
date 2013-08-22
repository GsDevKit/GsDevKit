*ob-standard-cmds
browseIt: aSymbol
	| entry |
	entry := self environment at: aSymbol ifAbsent: [nil].
	entry ifNil: [^ self implementorsOfIt: aSymbol].
	(entry isKindOf: Class) ifFalse: [entry := entry class].
	OBCodeBrowser defaultBrowserClass openOnClass: entry.
	^ true