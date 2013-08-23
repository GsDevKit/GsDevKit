commands
fileOut
	| file |
	file := GsFile openWrite: './', self theNonMetaClass name asString, '.gs'.
	self theNonMetaClass fileOutClassOn: file.
	file close.