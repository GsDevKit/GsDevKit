versions
versionNamesForPackageNamed: aString
	^ self repositories gather: [:repo |
		[repo versionNamesForPackageNamed: aString] on: Error do: [#()]]