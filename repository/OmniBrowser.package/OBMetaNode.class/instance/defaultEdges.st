metagraph
defaultEdges
	^ self edges select: [ :e | e isDefaultNavigation ]