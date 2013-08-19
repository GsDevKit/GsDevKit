name utilities
nameForVer: versionName base: baseName
	| baseId |
	baseId := versionName = baseName
		ifTrue: [ baseName versionNumber asString ]
		ifFalse:
			[ versionName packageName = baseName packageName
				ifTrue: [ baseName author, '.', baseName versionNumber asString ]
				ifFalse: [ '@' , baseName ] ].
	^ versionName versionName , '(' , baseId , ')'