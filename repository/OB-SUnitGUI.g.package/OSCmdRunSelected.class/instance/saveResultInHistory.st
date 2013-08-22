history
saveResultInHistory
	(result respondsTo: #dispatchResultsIntoHistory)
		ifTrue: [ result perform: #dispatchResultsIntoHistory ]