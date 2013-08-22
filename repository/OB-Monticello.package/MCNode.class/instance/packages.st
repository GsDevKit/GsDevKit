navigating
packages
	^(MCWorkingCopy allManagers asSortedCollection:
		[ :a :b | a package name <= b package name ]) collect: [:ea | ea asNode]