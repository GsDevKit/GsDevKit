Instance Creation
on: aCollection from: firstIndex to: lastIndex 
	"Answer a new instance of the receiver, streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self on: (aCollection copyFrom: firstIndex to: lastIndex)