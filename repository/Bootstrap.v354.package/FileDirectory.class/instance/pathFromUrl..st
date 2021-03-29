path access
pathFromUrl: aFileUrl
	^String streamContents: [ :s |
		aFileUrl path do: [ :p |
			s nextPut: self pathNameDelimiter.
			s nextPutAll: p ] ].