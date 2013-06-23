paths
pathForDirectory
	"Path using local file system's delimiter.  $\ or $:
	DOS paths with drive letters should not
	be prepended with a pathNameDelimiter even though
	they are absolute. Filename is left out."

	^String streamContents: [ :s |
		(self isAbsolute and: [self firstPartIsDriveLetter not])
			ifTrue: [ s nextPut: $/ ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPut: FileDirectory default pathNameDelimiter]]