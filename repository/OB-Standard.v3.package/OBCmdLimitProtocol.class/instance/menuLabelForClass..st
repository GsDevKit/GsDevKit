private
menuLabelForClass: aClass 
	| content |
	content := (self isLimitClass: aClass) 
				ifTrue: [aClass name asText addAttribute: TextEmphasis bold]
				ifFalse: [aClass name].
	^ content