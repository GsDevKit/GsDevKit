user interface
yellowButtonMenu
	^ Array streamContents:
		[:stream |
		stream 	
			nextPut: (Array with: 'find...(f)' translated with: #find);
			nextPut: (Array with: 'find again (g)' translated with: #findAgain);
			nextPut: (Array with: 'set search string (h)' translated with: #setSearchString);
			nextPut: #-;
			nextPut: (Array with: 'do again (j)' translated with: #again);
			nextPut: (Array with: 'undo (z)' translated with: #undo);
			nextPut: #-;
			nextPut: (Array with: 'copy (c)' translated with: #copySelection);
			nextPut: (Array with: 'cut (x)' translated with: #cut);
			nextPut: (Array with: 'paste (v)' translated with: #paste);
			nextPut: (Array with: 'paste...' translated with: #pasteRecent);
			nextPut: #-;
			nextPut: (Array with: 'do it (d)' translated with: #doIt);
			nextPut: (Array with: 'print it (p)' translated with: #printIt);
			nextPut: (Array with: 'inspect it (i)' translated with: #inspectIt);
			nextPut: (Array with: 'explore it (I)' translated with: #exploreIt);
			nextPut: (Array with: 'debug it' translated with: #debugIt);
			nextPut: #-;
			nextPut: (Array with: 'accept (s)' translated with: #accept);
			nextPut: (Array with: 'cancel (l)' translated with: #cancel)]