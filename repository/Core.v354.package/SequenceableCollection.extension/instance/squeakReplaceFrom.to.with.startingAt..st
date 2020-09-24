*core-squeak
squeakReplaceFrom: startIndex to: stopIndex with: aSeqCollection startingAt: repIndex

"Replaces the elements of the receiver between the indexes startIndex and 
 stopIndex inclusive with the elements of aSeqCollection starting at startIndex. 
 Returns the receiver....Vanilla algorithm is _required_"

| j |
j := repIndex.    
startIndex to: stopIndex do: [:i|
  self at: i put: (aSeqCollection at: j).
  j := j + 1.
  ].

^ self.