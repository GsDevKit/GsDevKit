accessing
workingCopy
	^ MCWorkingCopy allManagers detect: [:ea | ea package name = self packageName asString] ifNone: [parent ifNotNil: [parent workingCopy]]