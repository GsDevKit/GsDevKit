driver
writeInstallMasterFor: aVersionString extension: extension path: aPath repositoryMap: repositoryMap repositoryPath: aRepositoryPath

	| stream file |
	stream := WriteStream on: String new.

	stream 
		nextPutAll: '# This script was generated using an expression much like the following: '; cr;
		nextPutAll: '#'; cr;
		nextPutAll: '#    ', self class name asString; cr;
		nextPutAll: '#    	release: ', self configuration name asString; cr;
		nextPutAll: '#    	version: ', aVersionString printString; cr;
		nextPutAll: '#    	path: ', aPath printString; cr;
		nextPutAll: '#    	repositoryPath: ', aRepositoryPath printString, '.'; cr;
		nextPutAll: '#'; cr.

	self installScriptFor: aVersionString repositoryMap: repositoryMap on: stream.

	file := (ServerFileDirectory on: aPath) 
		forceNewFileNamed: 'installMaster', extension, '.topaz'.
	file nextPutAll: stream contents.
	file close.
