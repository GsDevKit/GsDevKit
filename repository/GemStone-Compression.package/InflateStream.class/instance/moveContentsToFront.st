private
moveContentsToFront
	"Move the decoded contents of the receiver to the front so that we have enough space for decoding more data."
	| delta |
	readLimit > 32768 ifTrue:[
		delta := readLimit - 32767.
		itsCollection 
			replaceFrom: 1 
			to: itsCollection size - delta + 1 
			with: itsCollection 
			startingAt: delta.
		position := position - delta + 1.
		readLimit := readLimit - delta + 1].