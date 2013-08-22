filtering
displayString: aString forParent: pNode child: cNode
	^ ((rescued at: pNode ifAbsent: [#()]) includes: cNode)
		ifTrue: [aString asText addAttribute: TextEmphasis struckOut]
		ifFalse: [aString]