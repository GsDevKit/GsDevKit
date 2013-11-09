interaction
registerInteractionRequest
  (Warning respondsTo: #'addDefaultHandler:')
    ifFalse: [ 
      Exception
        installStaticException: [ :ex :cat :num :args | 
          | result |
          "see GsInteractionRequest>>gsArguments"
          result := (args at: 1) perform: #'interactWith:' with: self.
          result	"resume with result of #interactWith:" ]
        category: GsInteractionRequest
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