*bootstrap
_description: aDescription

"Update the description of this Class.  Returns the argument."

| exd |
self commentStamp: self changeStamp.
(exd := extraDict) ifNil:[ 
  exd := SymbolDictionary new  .
  exd _objectSecurityPolicy: self objectSecurityPolicy .
  extraDict := exd 
].
exd at:#description put: aDescription .
^ aDescription