driver
writeBootstrapMasterFor: aVersionString extension: extension path: aPath 
	| stream file |
	stream := WriteStream on: String new.

	self bootstrapScriptFor: aVersionString on: stream.

	file := (ServerFileDirectory on: aPath) 
		forceNewFileNamed: 'bootstrapMaster', extension, '.topaz'.
	file nextPutAll: stream contents.
	file close.
