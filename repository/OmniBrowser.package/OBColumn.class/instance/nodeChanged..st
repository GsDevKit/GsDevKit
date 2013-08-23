updating
nodeChanged: ann 
	(self listHolder includesNode: ann node) ifTrue: [self changed: #list]