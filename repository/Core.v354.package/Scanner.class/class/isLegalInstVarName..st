testing
isLegalInstVarName: cleansedString
  "Answer whether aString is a legal instance variable name."

  ^ ((self isLiteralSymbol: cleansedString)
    and: [ (cleansedString includes: $:) not ])
    and: [ (self inviolateInstanceVariableNames includes: cleansedString) not ]