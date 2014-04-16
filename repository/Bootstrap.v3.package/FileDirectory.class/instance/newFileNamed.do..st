stream creation
newFileNamed:localFileName do: aBlock
	| file |
	file := self newFileNamed: localFileName.
	^ [ aBlock value: file ] ensure: [ file close ]