*core-squeak
_portableIsIndexable
  "relatively ugly hack to compensate for the fact that GemStone inherits the isIndexable attribute from it's superclass and Squeak/Pharo do not"

  self isIndexable
    ifFalse: [ ^ false ].
  (self inheritsFrom: AbstractException)
    ifTrue: [ ^ false ].
  (self inheritsFrom: Dictionary)
    ifTrue: [ ^ false ].
  (#(#'UrlArgumentList' #'UUID' #'WordArray' #'ArrayedCollection' #'RunArray' #'Text' #'ActionSequence')
    includes: self name)
    ifTrue: [ ^ false ].
  ^ true