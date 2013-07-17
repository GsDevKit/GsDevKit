*monticello
_mcDefinitionType
  | type opts superC |
  superC := self superClass .
  (self isBytes and:[ superC isBytes not ]) ifTrue:[ 
    type := #bytes
  ] ifFalse:[
    (self isIndexable and:[ superC isIndexable not]) ifTrue:[
      type := #variable 
    ] ifFalse:[
      type := #normal  
  ]].
  opts := self _nonInheritedOptions .
  ^ opts size > 0 ifTrue:[ { type . opts } ] ifFalse:[ type ] . 
