*monticello
_mcDefinitionType
  | type opts superC |
  superC := self superClass.
  self isBytes
    ifTrue: [ type := #'bytes' ]
    ifFalse: [ 
      self _portableIsIndexable
        ifTrue: [ type := #'variable' ]
        ifFalse: [ type := #'normal' ] ].
  opts := self _nonInheritedOptions.
  ^ opts size > 0
    ifTrue: [ 
      {type.
      opts} ]
    ifFalse: [ type ]