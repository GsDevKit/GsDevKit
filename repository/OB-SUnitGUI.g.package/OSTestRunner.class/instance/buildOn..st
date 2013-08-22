building
buildOn: aBuilder 
	"build in reverse order. navigationPanel needs to be _before_ the buttons in the panels list so that a new selection is reflected correctly in the state of the update buttons, however, I want the button panel layed out _before_ the navigationPanel"
	
	^aBuilder window: self
		with: 
			[ self buildGroup: panels reverse on: aBuilder]