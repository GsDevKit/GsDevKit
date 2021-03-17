*core-squeak
compileMethod: source category: cat
  | errorString |
  [ 
  self
    compileMethod: source
    category: cat
    using: GsSession currentSession symbolList ]
    onException: CompileError
    do: [ :ex | 
      errorString := GsNMethod
        _sourceWithErrors: ex errorDetails
        fromString: ex sourceString ].
  errorString
    ifNotNil: [ 
      self
        error:
          'Compile error in Class: ' , self name printString , ' method: ' , errorString.	"if user continues from the error, we return the errorString"
      ^ errorString ].
  ^ nil	"return nil to indicate no errors"