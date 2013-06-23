content
processingInstruction: piName data: dataString
	| newElement |
	newElement _ XMLPI target: piName data: dataString.
	self top addElement: newElement