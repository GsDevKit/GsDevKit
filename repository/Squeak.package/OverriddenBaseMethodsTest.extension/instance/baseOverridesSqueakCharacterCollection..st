*squeak
baseOverridesSqueakCharacterCollection: known

	self 
		addTo: known 
		forBehavior: CharacterCollection 
		overrides: {
			#asNumber -> 'asNumber

"Returns the receiver converted to a kind of number.  If the receiver contains
 all digits (with optional radix notation), returns a kind of Integer.  If the
 receiver has a slash, returns a Fraction.  Otherwise conversion to a Float is
 attempted.  An error may result if the receiver does not contain the proper
 format for a kind of Number."

| v zero nine str idx |
str := self trimWhiteSpace.
str size == 0 ifTrue: [ ^0 ].
zero := $0 asciiValue.
nine := $9 asciiValue.
idx := 1.
(str at: 1) = $- ifTrue: [
  idx := 2
]
ifFalse: [
  (str at: 1) = $+ ifTrue: [
    idx := 2
  ]
].
idx to: str size do: [:i |
  v := (str at: i) asciiValue.
  (zero <= v _and: [v <= nine]) ifFalse: [
    (v == ($# asciiValue) _or: [ v == $r asciiValue ]) ifTrue: [
      ^ Integer fromString: str .
    ] ifFalse:[
      v == ($/ asciiValue) ifTrue: [
        ^Fraction fromStream: (ReadStream on: str)
      ].
      ^Float fromStream: (ReadStream on: str)
    ]
  ]
].
^Integer fromString: str
'.
			#sameAs: -> 'sameAs: aCharCollection

"Returns true if the receiver is equivalent to aCharCollection.  The receiver
 is equivalent to aCharCollection if the receiver contains the same Characters
 as aCharCollection regardless of case or internal representation.  For
 example, if $a is in aCharCollection, it is equivalent to any representation
 of an ''a'' in the receiver''s character set."

(aCharCollection isKindOf: CharacterCollection)
  ifFalse: [ ^false ].

(self size ~~ aCharCollection size)
  ifTrue: [ ^false ].

1 to: (self size)
  do: [:i | ((self at: i) isEquivalent: (aCharCollection at: i))
            ifFalse: [ ^false ].
      ].
^true
'.
			#isEquivalent: -> 'isEquivalent: aCharCollection

"Returns true if the receiver is equivalent to aCharCollection.  The receiver
 is equivalent to aCharCollection if the receiver contains the same Characters
 as aCharCollection regardless of case or internal representation.  For
 example, if $a is in aCharCollection, it is equivalent to any representation
 of an ''a'' in the receiver''s character set."

"Note: This method behaves the same as #sameAs:"

^ self sameAs: aCharCollection
'.
			#equalsNoCase: -> 'equalsNoCase: aCharCollection
    "Returns true if the receiver is equivalent to aCharCollection.
    The receiver is equivalent to aCharCollection if the receiver
    contains the same Characters as aCharCollection regardless of case
    or internal representation.  For example, if $a is in
    aCharCollection, it is equivalent to any representation of an ''a''
    in the receiver''s character set."

    ^self sameAs: aCharCollection
'.
			#copyFrom:to: -> 'copyFrom: startIndex to: stopIndex

"Returns a new SequenceableCollection containing the elements of the receiver
 between startIndex and stopIndex, inclusive.  The result is of the same class
 as the receiver, unless the receiver is a Symbol or DoubleByteSymbol,
 in which case the result class is respectively String or DoubleByteString.

 Both startIndex and stopIndex must be positive integers not larger than the
 size of the receiver, with startIndex <= stopIndex.
 If startIndex > stopIndex and both are positive, an empty collection is returned.
 "

| result |
(startIndex > stopIndex) ifTrue: [ 
  stopIndex < 0 ifTrue:[ self _error: #rtErrBadCopyFromTo args: #[stopIndex] ].
  ^ self class new 
].
(startIndex < 1)
   ifTrue: [ ^ self _errorIndexOutOfRange: startIndex].

((stopIndex > self size) _or: [(stopIndex < 0)])
   ifTrue: [ ^ self _errorIndexOutOfRange: stopIndex].

result := (self class _newString: (stopIndex - startIndex + 1)).
self copyFrom: startIndex to: stopIndex into: result startingAt: 1.
^ result
'.
			#matchesAnyOf: -> 'matchesAnyOf: aCollectionOfCharacterColls

"Returns true if the receiver returns true to the message match: with any of
 the objects in the given collection; returns false otherwise.  Examples:

   ''xyz'' matchesAnyOf: #(''xyz'' ''abc*'')
     true
   ''xyz'' matchesAnyOf: #(''ayz'' ''abc'')
     false
   ''x#z'' matchesAnyOf: #(''x@z'' ''*'')
     false

 The class JISString does not support this method."

aCollectionOfCharacterColls do: [:coll |
  (self match: coll) ifTrue: [ ^true ]
].
^false
'.
			#subStrings: -> 'subStrings: aCharacter
  "Returns an Array of CharacterCollections in which each element represents a
   substring separated by aCharacter.  The result will include empty substrings when
   two adjacent separators exist, as well as if a separator is the first or last element."

  | result startIndex endIndex |
  result := Array new.
  startIndex := 1.

  [ endIndex := self indexOf: aCharacter startingAt: startIndex.
    endIndex == 0
  ] whileFalse: [
    (endIndex == startIndex) ifTrue:[ result add: self class _newString ].
    (endIndex > startIndex) ifTrue:[
       result add: (self copyFrom: startIndex to: endIndex - 1)
    ].
    startIndex := endIndex + 1
  ].
  (startIndex > self size)
     ifTrue:[ result add:  self class _newString]
     ifFalse:[ result add: (self copyFrom: startIndex to: self size)].

  ^ result.
'.
			#asInteger -> 'asInteger

"Returns an Integer whose value is represented by the receiver."

^ Integer fromString: self
'.
	}