interaction
registerInteractionRequest
  (Warning respondsTo: #'addDefaultHandler:')
    ifFalse: [ 
      Exception
        installStaticException: [ :ex :cat :num :args | (ex perform: #'interaction') perform: #'interactWith:' with: self ]
        category: GemStoneError
        number: 121001.
      ^ self ].
  GsInteractionRequest defaultHandlers isEmpty
    ifTrue: [ 
      "signalled from GemStone base interactions"
      GsInteractionRequest
        addDefaultHandler: [ :ex | 
          | response |
          response := (ex perform: #'interaction')
            perform: #'interactWith:'
            with: self.
          ex response: response ] ]