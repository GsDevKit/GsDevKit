testing
includesVersionNamed: aString 
	| versionName |
	versionName := aString asMCVersionName.
	self repositoriesDo:
		[ : ea | (ea includesVersionNamed: versionName) ifTrue: [ ^ true ] ].
	^ false