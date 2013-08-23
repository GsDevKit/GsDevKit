services
fileOut
	| file |
	file := GsFile openWrite: './', name asString, '.gs'.
	SystemOrganizer new fileOutCategory: name on: file.
	file close.