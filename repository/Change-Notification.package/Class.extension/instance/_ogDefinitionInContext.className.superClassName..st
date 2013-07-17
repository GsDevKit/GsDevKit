*change-notification
_ogDefinitionInContext: aUserProfile className: className superClassName: superClassName

"Returns a description of the receiver using object names taken from the given
 UserProfile. Crafted for OG-Standard browsers"

| result anArray lfsp newByteSubclass
  aConstraint firstElement inv civs |

result := String new.
result addAll: superClassName.

lfsp := (Character lf asString) addAll: String tab; yourself.

newByteSubclass := false.

(self isBytes _and: [superClass isBytes not]) ifTrue: [
  result addAll: ' byteSubclass: '''; addAll: className; addLast: $'.
  newByteSubclass := true.
]
ifFalse: [
  self _portableIsIndexable ifTrue: [
    result addAll: ' indexableSubclass: '''; addAll: className; addLast: $'.
  ]
  ifFalse: [
      self isTransientDB ifTrue: [
      result addAll: ' transientSubclass: '''; addAll: className; addLast: $'.
    ]
    ifFalse: [
      result addAll: ' subclass: '''; addAll: className; addLast: $'.
    ]
  ].
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

" poolDictionaries: #[ <list of dictionary names> ] "

inv := poolDictionaries isKindOf: InvariantArray.
result addAll: lfsp; addAll: 'poolDictionaries: #'.

result add: (inv ifTrue: [ $( ] ifFalse: [ $[ ]).

firstElement := true.
self sharedPools do: [:each |
  firstElement ifFalse: [ result addAll: ', '].  "separate with commas"
  anArray := aUserProfile dictionaryAndSymbolOf: each.
  anArray == nil
        ifTrue: [ | d |
            (d := aUserProfile objectNamed: each name) ~~ nil 
			ifTrue: [ 
				((d isKindOf: Class) and: [d isSubclassOf: SharedPool])
					ifTrue: [ result addAll: ( each name, ' _classVars')  ]
					ifFalse: [ result addAll: ' "(not named)" ' ]
			]
			ifFalse: [result addAll: ' "(not named)" ' ]
        ]
        ifFalse: [ result addAll: (anArray at: 2)].
  firstElement := false.
].

result add: (inv ifTrue: [ $) ] ifFalse: [ $] ]).

"inDictionary: <name of Dictionary>"

result addAll: lfsp; addAll: 'inDictionary: '.
anArray := System myUserProfile dictionaryAndSymbolOf: self.
(anArray == nil or: [ (anArray at: 1) == GsPackagePolicy current homeSymbolDict])
	ifTrue: [ result addAll: '''''' ]
	ifFalse: [
		anArray := System myUserProfile dictionaryAndSymbolOf: (anArray at: 1).
		anArray == nil 
			ifTrue: [ result addAll: '''''' ]
			ifFalse: [ result addAll: (anArray at: 2) asString printString ]].

"category: <name of class category>"

result addAll: lfsp; addAll: 'category: '.
result addAll: self category printString.

result add: Character lf.
^result