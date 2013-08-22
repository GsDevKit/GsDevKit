callbacks
selection: anInteger
	OBUpdateConcentrator default 
		recordUpdatesDuring: [
			self listHolder selection: anInteger.
			self signalSelectionChanged.
			self changed: #selection.
			switch currentNode: self selectedNode ]