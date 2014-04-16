accessing
finalBlockHomeFor: anOGStackFrame
  (self receiver isKindOf: ExecBlock)
    ifFalse: [ 
      anOGStackFrame frameSelf == nil
        ifTrue: [ 
          anOGStackFrame method == self method
            ifTrue: [ ^ self ] ]
        ifFalse: [ 
          (anOGStackFrame frameSelf == self receiver
            and: [ anOGStackFrame method == self method ])
            ifTrue: [ ^ self ] ] ].
  self sender == nil
    ifTrue: [ ^ self ].
  ^ self sender finalBlockHomeFor: anOGStackFrame