actions
removeUnreferencedClasses
  | symbolList |
  symbolList := GsSession currentSession symbolList.
  self dictionary keys
    do: [ :cl | 
      (symbolList objectNamed: cl thisClass name) isNil
        ifTrue: [ self dictionary removeKey: cl ] ]