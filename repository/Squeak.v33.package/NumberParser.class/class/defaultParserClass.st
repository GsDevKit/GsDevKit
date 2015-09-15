instance creation
defaultParserClass
	DefaultParserClass ifNil: [ DefaultParserClass := SqNumberParser ].
 	^ DefaultParserClass
