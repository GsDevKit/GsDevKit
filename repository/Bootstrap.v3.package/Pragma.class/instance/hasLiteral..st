testing
hasLiteral: aLiteral
  ^ self keyword == aLiteral or: [ self arguments refersToLiteral: aLiteral ]