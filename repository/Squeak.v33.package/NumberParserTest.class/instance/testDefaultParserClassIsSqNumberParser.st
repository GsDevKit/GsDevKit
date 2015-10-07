tests
testDefaultParserClassIsSqNumberParser
  "The defaultParserClass of NumberParser is SqNumberParser, but can be changed"

  Transcript
    cr;
    show:
        'testDefaultParserClassIsSqNumberParser: '
            , NumberParser defaultParserClass printString , ' -> '
            , defaultParserClass printString.
  self assert: NumberParser defaultParserClass == defaultParserClass.
  NumberParser defaultParserClass: ExtendedNumberParser.
  self assert: NumberParser defaultParserClass == ExtendedNumberParser.
  NumberParser defaultParserClass: SqNumberParser.
  self assert: NumberParser defaultParserClass == SqNumberParser