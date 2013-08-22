execution
execute

	self pickAncestorVersionInfo ifNotNilDo: [:info |
		(self repositoryGroup versionWithInfo: info
			ifNone: [^self inform: 'No repository found for ', info name]
		) load].
	self refresh