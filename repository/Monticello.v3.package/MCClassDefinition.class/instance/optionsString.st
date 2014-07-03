printing
optionsString
   | ty opts str |
   (ty := self type) _isArray ifFalse:[ ^ '#()' ].
   opts := ty at: 2 .
   str := '#( ' copy  .
   opts do:[:aSym | str add: aSym ; add: $  ].
   str add: $) .
   ^ str