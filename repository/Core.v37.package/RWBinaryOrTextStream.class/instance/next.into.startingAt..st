accessing
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
	0 to: n-1 do:[:i|
		self atEnd ifTrue: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		obj := self next.
		self isBinary ifTrue: [ obj := obj asInteger ].
		aCollection at: startIndex+i put: obj].
	^aCollection