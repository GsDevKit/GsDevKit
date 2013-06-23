private
fileOutClasses: order on: stream

"Writes out code on the given stream that creates the given classes"
| class str lf head term any up |

order size == 0 ifTrue: [ ^self ].
head := 'doit
'.
term := '
' , '%
'.

lf := Character lf.
up := System myUserProfile.
1 to: order size do: [:j | | ar dict nm |
  class := order at: j.
  class fileOutPreClassOn: stream.
  ar := up dictionaryAndSymbolOf: class.
  dict := ar at: 1.
  nm := dict name.
  stream nextPutAll: head;
          nextPutAll:
    (class _modifiableDefinitionInDictionary: dict named: nm );
    nextPut: $.;
    nextPutAll: term
].

"now write out constraints and make non-modifiable classdict non-modifiable "
1 to: order size do: [:k | | ar dict |
  class := order at: k.
  any := false.
  ar := up dictionaryAndSymbolOf: class.
  dict := ar at: 1.
  str := class _constraintCreationExpressionIn: dict.
  str size > 0 ifTrue: [
    stream nextPutAll: head.
    stream nextPutAll: (dict keyAtValue: class); nextPut: $ ;
      nextPutAll: str;
      nextPut: $.; nextPut: lf.
    any := true.
  ].
  class isModifiable ifFalse: [
    any ifFalse: [
      stream nextPutAll: head.
      any := true
    ].
    stream nextPutAll: (dict keyAtValue: class); nextPutAll: ' immediateInvariant.'.
  ].
  any ifTrue: [
    stream nextPutAll: term
  ]
].