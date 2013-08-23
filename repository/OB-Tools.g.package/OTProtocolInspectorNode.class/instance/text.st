accessing
text
	^ [ (self theClass sourceCodeAt: self selector) asText ]
		on: Error, Halt 
		do: [:ex | | text |
			text := '<error in printstring>' asText.
			text
				addAttribute: TextColor red
				from: 1
				to: text size.
			text]