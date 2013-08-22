displaying
displayString
	^ [(self theClass includesLocalSelector: selector) 
		ifTrue: [super displayString]
		ifFalse: [super displayString asText addAttribute: TextEmphasis italic]] 
			on: MessageNotUnderstood
			do: [:ex |
				ex selector ~~ #includesLocalSelector: ifTrue: [ ex pass ].
				^super displayString]