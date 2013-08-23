accessing
defaultEdgesForNode: aNode 
	^ filters 
		inject: self defaultEdges
		into: [:list :filter | filter edgesFrom: list forNode: aNode].