navigation
files
	^ self directory fileNames collect: [ :each | 
		(OTFileNode on: self)
			setName: each;
			yourself ]