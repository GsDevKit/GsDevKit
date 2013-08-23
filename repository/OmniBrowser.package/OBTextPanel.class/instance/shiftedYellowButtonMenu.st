user interface
shiftedYellowButtonMenu
	^ Array streamContents:
		[:stream |
		stream
			nextPut: (Array with: 'explain' translated with: #explain);
			nextPut: #-;
			nextPut: (Array 
						with: 'selectors containing it (W)' translated 
						with: #methodNamesContainingIt);
			nextPut: (Array 
						with: 'method strings with it (E)' translated 
						with: #methodStringsContainingit);
			nextPut: (Array 
						with: 'method source with it' translated 
						with: #methodSourceContainingIt);
			nextPut: (Array 
						with: 'class names containing it' translated 
						with: #classNamesContainingIt);
			nextPut: (Array 
						with: 'class comments with it' translated 
						with: #classCommentsContainingIt);
			nextPut: (Array 
						with: 'change sets with it' translated 
						with:  #browseChangeSetsWithSelector);
			nextPut: #-;
			nextPut: (Array with: 'save contents to file...' translated with: #saveContentsInFile);
			nextPut: #-;
			nextPut: (Array with: 'more...' translated with: #yellowButtonActivity)]