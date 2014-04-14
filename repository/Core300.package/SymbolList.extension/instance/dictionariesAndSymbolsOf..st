*core300
dictionariesAndSymbolsOf: anObject

"Returns an Array, possibly empty, of the form { { aDictionary . aSymbol } ... }.
 Searches the receiver for Associations whose value is identical to
 anObject and returns information for all matching Associations. "

| res |
res := { } .
1 to: self size do:[ :j | 
  (self at: j) ifNotNil:[ :aDict |
    aDict associationsDo:[ :assoc |
      assoc _value == anObject ifTrue:[ res add: { aDict . assoc key }]
    ].
  ].
].
^ res
