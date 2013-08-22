navigating
workingCopyFor: aString
	| all |
	all _ MCWorkingCopy allManagers select: [:ea | aString beginsWith: ea package name].
	all isEmpty ifTrue: [^ nil].
	^ (all detectMax: [:ea | ea package name size])