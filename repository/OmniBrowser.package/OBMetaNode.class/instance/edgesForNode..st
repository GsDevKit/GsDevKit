accessing
edgesForNode: aNode 
     "do not filter tree edges for the moment"
	^(edges select: [:ea | ea isTreeEdge]), (filters 
		inject: (edges reject: [:ea | ea isTreeEdge]) 
		into: [:list :filter | filter edgesFrom: list forNode: aNode]).