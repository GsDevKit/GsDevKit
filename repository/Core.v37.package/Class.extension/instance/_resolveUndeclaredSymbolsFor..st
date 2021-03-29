*core-squeak-subclass creation
_resolveUndeclaredSymbolsFor: aSymbol
  | undefinedSymbols |
  undefinedSymbols := GsSession currentSession symbolList objectNamed: #UndefinedSymbols.
  undefinedSymbols ifNotNil:[ | coll |
      coll := undefinedSymbols at: aSymbol otherwise: nil . 
      coll ifNotNil:[ coll do: [:assoc |  | k v |
	 ( (k := assoc key) includesSelector: (v := assoc value)) ifTrue: [ 
            k _recompile: v 
         ]
      ]].
      coll := (undefinedSymbols at: aSymbol otherwise: nil) size == 0 ifTrue: [ 
        undefinedSymbols removeKey: aSymbol ifAbsent:[]
      ].
  ].