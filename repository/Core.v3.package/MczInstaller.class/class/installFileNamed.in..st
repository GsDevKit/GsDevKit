installing
installFileNamed: aFileName in: aSymDict
	| file stream |
	file := GsFile open: aFileName mode: 'rb'.
	stream := RWBinaryOrTextStream on: file contents.
	stream name: aFileName.
	file close.
    self installStream: stream in: aSymDict