versions
versionNamed: aString
	self repositories do: [:repo | | version |
		version := [repo versionNamed: aString] on: Error do: [nil].
		version ifNotNil: [:v | ^ v]].
	self error: 'No repositories contain version ', aString.