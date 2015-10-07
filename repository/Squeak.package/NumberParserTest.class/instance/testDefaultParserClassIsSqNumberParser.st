as yet unclassified
testDefaultParserClassIsSqNumberParser
	"The defaultParserClass of NumberParser is SqNumberParser, but can be changed"
	
	self assert: NumberParser defaultParserClass == SqNumberParser.
	NumberParser defaultParserClass: ExtendedNumberParser.
	self assert: NumberParser defaultParserClass == ExtendedNumberParser.