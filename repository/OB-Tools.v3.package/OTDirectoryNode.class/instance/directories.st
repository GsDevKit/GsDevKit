navigation
directories
	^ self directory directoryNames collect: [ :each | 
		(OTDirectoryNode on: self)
			setDirectory: (self directory directoryNamed: each);
			yourself ]