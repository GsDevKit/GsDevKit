initialization
initializeOn: anObject
	announcer := OBAnnouncer new.
	panel := self panelClass inBrowser: self on: anObject