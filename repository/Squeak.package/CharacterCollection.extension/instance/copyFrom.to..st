*squeak
copyFrom: startIndex to: stopIndex
"Override copyFrom:to: to conform to ANSI standard (bug 35799)"
"Returns a new SequenceableCollection containing the elements of the receiver
 between startIndex and stopIndex, inclusive.  The result is of the same class
 as the receiver, unless the receiver is a Symbol or DoubleByteSymbol,
 in which case the result class is respectively String or DoubleByteString.

 Both startIndex and stopIndex must be positive integers not larger than the
 size of the receiver, with startIndex <= stopIndex."

| result |

(startIndex > stopIndex)
  ifTrue: [ ^ self class _newString: 0].

(startIndex < 1)
   ifTrue: [ ^ self _errorIndexOutOfRange: startIndex].

((stopIndex > self size) _or: [(stopIndex < 0)])
   ifTrue: [ ^ self _errorIndexOutOfRange: stopIndex].

result := (self class _newString: (stopIndex - startIndex + 1)).
self copyFrom: startIndex to: stopIndex into: result startingAt: 1.
^ result