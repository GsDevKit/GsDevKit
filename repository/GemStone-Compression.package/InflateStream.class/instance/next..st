accessing
next: anInteger
  "Answer the next anInteger elements of my collection.  overriden for simplicity"

  | newArray |
  "try to do it the fast way"
  position + anInteger <= readLimit
    ifTrue: [ 
      newArray := itsCollection copyFrom: position to: position + anInteger - 1.
      position := position + anInteger.
      ^ newArray ].
  newArray := itsCollection species new: anInteger.	"oh, well..."
  1 to: anInteger do: [ :index | 
    newArray
      at: index
      put: (self next ifNil: [ ^ newArray copyFrom: 1 to: index - 1 ]) ].
  ^ newArray