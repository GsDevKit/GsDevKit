printing
printCanonicalOn: aStream

	| writer |
	writer _ XMLWriter on: aStream.
	writer canonical: true.
	self printXMLOn: writer