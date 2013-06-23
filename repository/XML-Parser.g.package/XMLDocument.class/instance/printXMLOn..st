printing
printXMLOn: writer
	version ifNotNil: [writer xmlDeclaration: self version encoding: self encoding].
	super printXMLOn: writer