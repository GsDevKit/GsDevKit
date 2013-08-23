building
buildOn: aBuilder 
	^aBuilder scroller: self with: [columns do: [:ea | ea buildOn: aBuilder]]