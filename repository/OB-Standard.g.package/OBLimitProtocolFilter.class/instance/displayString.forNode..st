filtering
displayString: aString forNode: aNode
	(aNode isKindOf: OBMethodNode) ifFalse: [^ aString].
	^ aNode theClass = theClass
		ifTrue: [aString asText addAttribute: TextEmphasis bold]
		ifFalse: [aString].