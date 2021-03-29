accessing
next: count

"Returns the next count elements in the receiver's collection."

| result |
self isBinary
	ifTrue: [ result := ByteArray new ]
	ifFalse: [result := itsCollection species new ].
count timesRepeat: [ 
	self atEnd ifTrue: [ ^ result ].
	result add: self next ].
^result