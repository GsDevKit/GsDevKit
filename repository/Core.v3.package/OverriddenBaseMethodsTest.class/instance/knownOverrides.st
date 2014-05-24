special cases
knownOverrides

  | knownOverrides |
  knownOverrides := IdentityDictionary new.
  (self class allSelectors select: [:ea | ea beginsWith: 'baseOverrides' ]) do:[:selector | 
    self perform: selector with: knownOverrides ].
  ^knownOverrides