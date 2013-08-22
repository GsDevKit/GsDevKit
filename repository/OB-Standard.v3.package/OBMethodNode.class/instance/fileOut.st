actions
fileOut
	| file |
	file := GsFile openWrite: './', (self theClass name , '-' , (selector asString copyReplaceAll: ':' with: '')), '.gs'.
	file nextPutAll: (self theClass fileOutMethod: selector).
	file close.