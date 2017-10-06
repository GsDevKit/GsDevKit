accessing
next

(position > readLimit)
   ifTrue: [ ^ self pastEndRead ].
position := position + 1.
^ itsCollection at: (position - 1)