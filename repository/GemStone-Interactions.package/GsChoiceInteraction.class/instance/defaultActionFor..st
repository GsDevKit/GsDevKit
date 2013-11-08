interacting
defaultActionFor: anInteractionRequest
  defaultActionBlock ifNil: [ ^ anInteractionRequest response: nil ].
  ^ super defaultActionFor: anInteractionRequest