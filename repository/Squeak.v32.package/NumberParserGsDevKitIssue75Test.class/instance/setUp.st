running
setUp
  "run tests using ExtendedNumberParser class"

  "https://github.com/GsDevKit/GsDevKit/issues/75"

  super setUp.
  NumberParser defaultParserClass: ExtendedNumberParser