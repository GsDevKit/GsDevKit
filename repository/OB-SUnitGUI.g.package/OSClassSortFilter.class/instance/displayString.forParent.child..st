as yet unclassified
displayString: aStringOrText forParent: pNode child: cNode

	| str |
	str := super displayString: aStringOrText forParent: pNode child: cNode.
	cNode isAbstractTestCase ifTrue: [ str := Text string: str attribute: TextEmphasis italic].
	^str