accessing
throughAll: matchCollection

"Returns a collection of objects from the receiver up to and including the
sequence of objects in the argument 'matchCollection', leaving the stream
positioned after the sequence.  If the sequence of objects is not found, this
returns the remaining contents of the receiver and leaves me positioned
at my end."

| numMatched numToMatch result  |

numMatched := 0.
result := self collectionSpecies new.
numToMatch := matchCollection size.
[self atEnd _or: [numMatched = numToMatch]]
     whileFalse:
           [self next = (matchCollection at: numMatched + 1)
                ifTrue: [numMatched := numMatched + 1]
          ifFalse: [self position: self position - numMatched - 1.
                        result add: self next.
                        numMatched := 0]
].

"add matched or partially matched chars"
self position: self position - numMatched.
numMatched timesRepeat: [result add: self next].

^ result.
