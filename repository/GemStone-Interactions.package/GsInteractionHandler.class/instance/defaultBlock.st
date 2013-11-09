accessing
defaultBlock
  defaultBlock
    ifNil: [ 
      defaultBlock := [ :interaction | 
      self
        error:
          'No handler defined for ' , interaction class name asString , ' interaction.' ] ].
  ^ defaultBlock