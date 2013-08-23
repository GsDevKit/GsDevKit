navigation
anyNavigation
	^ self hasDefaultNavigationEdge 
		ifTrue: [ OBDefaultEdgeNavigation new ]
		ifFalse: [ (self edges at: 1) navigation]