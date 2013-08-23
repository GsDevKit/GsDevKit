building
buildGroup: aCollection on: aBuilder 
	^aBuilder verticalGroupWith: [aCollection do: [:ea | ea buildOn: aBuilder]]