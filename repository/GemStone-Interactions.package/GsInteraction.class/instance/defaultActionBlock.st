accessing
defaultActionBlock
  defaultActionBlock
    ifNil: [ 
      ^ [ :interactionRequest | 
      (ObjectLogEntry interaction: self printString object: self) addToLog.
      nil ] ].
  ^ defaultActionBlock