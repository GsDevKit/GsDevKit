interacting
defaultActionFor: anInteractionRequest
  defaultActionBlock ifNil: [ ^ nil ].
  ^ super defaultActionFor: anInteractionRequest