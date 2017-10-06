initialization
rewindData
	super rewindData.
	stream := ReadStream on: contents.
	readDataRemaining := contents size.