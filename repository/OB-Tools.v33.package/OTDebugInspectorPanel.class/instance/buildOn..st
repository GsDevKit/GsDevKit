building
buildOn: aBuilder
	^ aBuilder horizontalGroupWith: [
		receiverInspector
			buildGroup: receiverInspector panels
			on: aBuilder.
		contextInspector
			buildGroup: contextInspector panels
			on: aBuilder ]