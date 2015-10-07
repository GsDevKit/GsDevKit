initialization
initializeOn: aStream
	super initializeOn: aStream.
	stream := aStream.
	stream addDependent: self