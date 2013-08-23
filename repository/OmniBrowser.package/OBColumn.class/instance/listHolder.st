list
listHolder
	^ listHolder ifNil: [ listHolder := OBList column: self ]