converting
on: aNode for: aRequestor 
	| inst |
	inst _ self copy.
	inst setTarget: aNode requestor: aRequestor.
	^ inst