accessing
packageName
	| allNames candidates |
	allNames _ MCWorkingCopy allManagers collect: [:ea | ea package name].
	candidates _ allNames select: [:ea | self name asString beginsWith: ea].
	candidates isEmpty ifTrue: [^ nil].
	^ candidates detectMax: [:ea | ea size]