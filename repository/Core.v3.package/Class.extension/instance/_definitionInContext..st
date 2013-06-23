*core-squeak-subclass creation
_definitionInContext: aUserProfile

"Returns a description of the receiver using object names taken from the given
 UserProfile."

| result newByteSubclass anArray lfsp firstElement poolDicts civs supercls |

result := String new.
supercls := self superClass .
result addAll: (supercls ifNil:[ 'nil' ] ifNotNil: [supercls name]).

newByteSubclass := false.
(lfsp := Character lf asString) addAll: '  ' .


(self isBytes and: [supercls isBytes not]) ifTrue: [
  result addAll: ' byteSubclass: '''; addAll: name; addLast: $'.
  newByteSubclass := true.
] ifFalse: [
  (self isIndexable and: [supercls isIndexable not]) ifTrue: [
    result addAll: ' indexableSubclass: '''; addAll: name; addLast: $'.
  ] ifFalse: [
    result addAll: ' subclass: '''; addAll: name; addLast: $'.
  ]
].

" instVarNames: #( <list of strings> ) "

newByteSubclass ifFalse: [
  result addAll: lfsp;
    addAll: 'instVarNames: #(';
    addAll: (self _instVarNamesWithSeparator: (lfsp , '                 '));
    add: $).
].

" classVars: #( <list of strings> ) "
result addAll: lfsp; addLast: 'classVars: #('.
self _sortedClassVarNames do: [:aKey |
  result addLast: $  . 
  (aKey includesValue: $') 
    ifTrue:[ result addAll: aKey _asSource ]
    ifFalse:[ result addAll: aKey ].
  ].
result addLast: $).

" classInstVars: #( <list of strings> ) "

result addAll: lfsp; addLast: 'classInstVars: #('.
civs := self class allInstVarNames.
civs removeFrom: 1 to: (self class superClass instSize).
civs do: [:civName |
  result addLast: $  .
  (civName includesValue: $') 
    ifTrue:[ result addAll: civName _asSource ]
    ifFalse:[ result addAll: civName ].
].
result addLast: $).

" poolDictionaries: { <list of dictionary names> } "
result addAll: lfsp; addAll: 'poolDictionaries: '.
(poolDicts := self sharedPools) size > 0 ifTrue:[
  result addAll: ' {' .
  firstElement := true.
  poolDicts do: [:each |
    firstElement ifFalse: [ result addAll: ' . '].  
    anArray := aUserProfile dictionaryAndSymbolOf: each.
    anArray ifNil: [ | d |
      (d := aUserProfile objectNamed: each name) ifNotNil:[ 
    ((d isKindOf: Class) and: [d isSubclassOf: SharedPool])
        ifTrue:[ result addAll: ( each name, ' _createClassVarsDict')  ]
        ifFalse:[ result addAll: '"(not named)"' ]
       ] ifNil:[ result addAll: '"(not named)"' ]
    ] ifNotNil: [ result addAll: (anArray at: 2)].
    firstElement := false.
  ].
  result addAll: ' }'
] ifFalse:[
  result addAll: '#()'
].

" inDictionary: <name of containing dictionary> "
result addAll: lfsp; addAll: 'inDictionary: '.
anArray := aUserProfile dictionaryAndSymbolOf: self.
anArray ifNil: [
  result addAll: '(class not in your dictionaries)'
] ifNotNil: [
  anArray := aUserProfile dictionaryAndSymbolOf: (anArray at: 1).
  anArray ifNil: [
    result addAll: '(dictionary not in your dictionaries)'
  ] ifNotNil: [
    result addAll: (anArray at: 2)
  ]
].

"category: <name of class category>"
classCategory ifNotNil:[
        result addAll: lfsp; addAll: 'category: '.
        result addAll: classCategory printString.
].

" constraints: omitted, no longer enforced in Gs64 "

"options:"
result add:  lfsp; add: self _optionsForDefinition ; 
       lf  .
^result
