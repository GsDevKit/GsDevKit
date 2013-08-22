building
buildOn: aBuilder 
	^ aBuilder window: self with: [ 
		aBuilder verticalGroupWith: [
			panel buildOn: aBuilder ] ]