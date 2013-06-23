printing
printXMLOn: writer
	self elementsDo: [:element | element printXMLOn: writer]