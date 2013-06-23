instance creation
parseDTDFrom: aStream
	| driver parser |
	driver _ SAXDriver on: aStream.
	driver validating: true.
	driver startParsingMarkup.
	parser _ self new driver: driver.
	parser startDocument.
	parser parseDocument.
	^parser