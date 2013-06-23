*core-squeak
compileMethod: source category: cat

   | result errorString |
   result := self compileMethod: source category: cat using: GsSession currentSession symbolList.
   result ifNotNil: [
      errorString := GsMethod _sourceWithErrors: result fromString: source.

      self error: 'Compile error in Class: ', self name printString, ' method: ', errorString .
      "if user continues from the error, we return the errorString"
      ^ errorString
   ].
   ^ nil  "return nil to indicate no errors"