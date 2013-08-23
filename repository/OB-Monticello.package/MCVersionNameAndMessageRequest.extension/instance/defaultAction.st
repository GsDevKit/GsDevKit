*ob-monticello
defaultAction
  | v m |
  v := OBTextRequest
    prompt: 'Please enter a name for your version:'
    template: suggestion.
  [ 
  v == nil
    ifTrue: [ ^ nil ].
  v = v encodeForHTTP ]
    whileFalse: [ 
      v := OBTextRequest
        prompt:
          v encodeForHTTP printString
            ,
              ' contains illegal characters. Please enter a valid name for your version:'
        template: suggestion ].
  m := OBMultiLineTextRequest
    prompt: 'Please enter a commit message:'
    template: ''.
  (m ~~ nil and: [ m isEmpty not ])
    ifTrue: [ 
      ^ {v.
      m} ].
  ^ nil