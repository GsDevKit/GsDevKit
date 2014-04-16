private
fileOutClasses: order on: stream
  "Writes out code on the given stream that creates the given classes"

  | class str lf head term any sl |
  order size == 0
    ifTrue: [ ^ self ].
  head := 'doit
'.
  term := '
'
    ,
      '%
'.
  lf := Character lf.
  sl := GsSession currentSession symbolList.
  1 to: order size do: [ :j | 
    | ar dict nm |
    class := order at: j.
    class fileOutPreClassOn: stream.
    ar := sl dictionaryAndSymbolOf: class.
    dict := ar at: 1.
    nm := dict name.
    stream
      nextPutAll: head;
      nextPutAll: (class _modifiableDefinitionInDictionary: dict named: nm);
      nextPut: $.;
      nextPutAll: term ].
  1 to: order size do: [ :k | 
    | ar dict |
    "now make non-modifiable classdict non-modifiable "
    class := order at: k.
    any := false.
    ar := sl dictionaryAndSymbolOf: class.
    dict := ar at: 1.
    class isModifiable
      ifFalse: [ 
        any
          ifFalse: [ 
            stream nextPutAll: head.
            any := true ].
        stream
          nextPutAll: (dict keyAtValue: class);
          nextPutAll: ' immediateInvariant.' ].
    any
      ifTrue: [ stream nextPutAll: term ] ]