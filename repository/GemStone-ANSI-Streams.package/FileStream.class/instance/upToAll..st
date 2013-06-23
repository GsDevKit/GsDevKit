accessing
upToAll: matchCollection 

"Returns a collection of objects from the receiver up to, but not including,
 the sequence of objects in the argument 'matchCollection', leaving the stream
 positioned to read the sequence.  If the sequence of objects is not found,
 this returns the remaining contents of the receiver and leaves the stream 
 positioned at the end." 

| numMatched numToMatch result | 

numMatched := 0. 
result := self collectionSpecies new. 
numToMatch := matchCollection size. 
[self atEnd or: [numMatched == numToMatch]] 
  whileFalse: 
    [self next = (matchCollection at: numMatched + 1) 
      ifTrue: [numMatched := numMatched + 1] 
      ifFalse: [self position: self position - numMatched - 1. 
    result add: self next. 
    numMatched := 0] 
    ]. 
"Position before any partial or complete match we might have found." 
self position: self position - numMatched. 

"If the match was not complete, must add any partially matched chars." 
numMatched ~~ numToMatch 
  ifTrue: [numMatched timesRepeat: [result add: self next]]. 

^ result. 
