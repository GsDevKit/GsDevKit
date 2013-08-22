actions
workingCopies
	^(MCWorkingCopy allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ])