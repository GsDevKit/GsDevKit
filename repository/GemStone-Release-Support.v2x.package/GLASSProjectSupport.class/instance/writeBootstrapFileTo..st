driver
writeBootstrapFileTo: aPath

	| stream file |
	stream := WriteStream on: String new.

	self bootstrapFileOutOn: stream.

	file := (ServerFileDirectory on: aPath) 
		forceNewFileNamed: 'Bootstrap', self bootstrapFileExtension, '.gs'.
	file nextPutAll: stream contents.
	file close.
