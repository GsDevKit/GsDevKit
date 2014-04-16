actions
removeUnreferencedClasses
  | symbolList |
  symbolList := GsSession currentSession symbolList.
  dictionary keys
    do: [ :cl | 
      (symbolList objectNamed: cl thisClass name) isNil
        ifTrue: [ dictionary removeKey: cl ] ]