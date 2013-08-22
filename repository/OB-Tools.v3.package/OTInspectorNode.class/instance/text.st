public
text
	^ [self value printString asText] 
		on: Error, Halt 
		do: [:ex | | text |
			text := self printStringErrorText: ex.
			text
				addAttribute: TextColor red
				from: 1
				to: text size.
			text]