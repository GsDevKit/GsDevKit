callbacks
listSelectionAt: anInteger put: aValue 

	OBUpdateConcentrator default 
		recordUpdatesDuring: [ ^self _listSelectionAt: anInteger put: aValue ]