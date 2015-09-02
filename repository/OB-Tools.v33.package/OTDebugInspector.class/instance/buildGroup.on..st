building
buildGroup: aCollection on: aBuilder 
	^ aBuilder horizontalGroupWith: [ aCollection do: [ :ea | ea buildOn: aBuilder ] ]