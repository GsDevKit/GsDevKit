stream creation
readOnlyFileNamed: localFileName do: aBlock
	| file |
	file := self readOnlyFileNamed: localFileName.
	^ [ aBlock value: file ] ensure: [ file close ]
