running
tearDownClasses
  #('GsDevKitIssue83B' 'GsDevKitIssue83A')
    do: [ :className | (Smalltalk at: className) ifNotNil: [ :cls | cls removeFromSystem ] ]