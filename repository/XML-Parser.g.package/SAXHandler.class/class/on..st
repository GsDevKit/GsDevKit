instance creation
on: aStream
	| driver parser |
	driver _ SAXDriver on: aStream.
	driver validating: true.
	parser _ self new driver: driver.
	^parser