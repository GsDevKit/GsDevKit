callbacks
labelString
	| label |
	label := self navigationPanel labelString.
	^label 
		ifNil: [self defaultLabel]
		ifNotNil: [label , ' - ', self defaultLabel]