execution
execute
  | current startFrame startMethod |
  current := self context.
  startMethod := current method.
  (current receiver isKindOf: ExecBlock)
    ifFalse: [ ^ self ].
  startFrame := current frameSelf.
  [ 
  current ~~ nil
    and: [ current receiver == startFrame and: [ current method == startMethod ] ] ]
    whileFalse: [ 
      self process completeStep: current.
      current := self process suspendedContext ].
  self update