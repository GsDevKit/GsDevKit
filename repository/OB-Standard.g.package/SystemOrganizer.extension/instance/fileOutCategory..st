*ob-standard
fileOutCategory: cat
	"Return a String containing the GemStone fileout for all the classes associated with the category ."

	| file |
	file := GsFile openWrite: './', cat asString, '.gs'.
	self fileOutCategory: cat on: file.
	file close.