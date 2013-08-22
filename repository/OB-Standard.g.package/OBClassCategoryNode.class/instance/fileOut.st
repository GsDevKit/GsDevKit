services
fileOut
	| file |
	file := GsFile openWrite: './', name asString, '.gs'.
	environment systemOrganization fileOutCategory: name on: file.
	file close.