content
characters: aString
	| newElement |
	newElement _ XMLStringNode string: aString.
	self top addContent: newElement.