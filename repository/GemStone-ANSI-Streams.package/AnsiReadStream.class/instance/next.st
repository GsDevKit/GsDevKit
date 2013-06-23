Accessing
next

"Returns the next object that the receiver can access for reading.  Generates
 an error if an attempt is made to read beyond the end of the stream."

(self atEnd)
   ifTrue: [ ^  nil ].
position := position + 1.
^ itsCollection at: (position - 1)
