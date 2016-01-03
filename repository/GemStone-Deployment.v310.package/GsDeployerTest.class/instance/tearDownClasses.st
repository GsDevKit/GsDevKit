running
tearDownClasses
  self issue83ClassNames
    do: [ :className | (Smalltalk at: className) ifNotNil: [ :cls | cls removeFromSystem ] ]