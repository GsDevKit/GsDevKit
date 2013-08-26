as yet unclassified
deleteFile
	| name |
	name := self fileName asString.
	(FileDirectory default fileExists: name)
		ifTrue: [FileDirectory default deleteFileNamed: name]