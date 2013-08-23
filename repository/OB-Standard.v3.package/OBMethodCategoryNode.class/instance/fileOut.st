services
fileOut
	| file |
	file := GsFile openWrite: './', (self theClass name , '-' , name asString), '.gs'.
	self theClass fileOutCategory: name on: file.
	file close.