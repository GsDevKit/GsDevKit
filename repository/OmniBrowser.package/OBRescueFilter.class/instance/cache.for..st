filtering
cache: aCollection for: aNode
	| cached missing |
	cached _ cache at: aNode ifAbsent: [^ self initCache: aCollection for: aNode].
	cache at: aNode put: aCollection.
	missing _ rescued at: aNode ifAbsent: [Set new].
	missing removeAllSuchThat: [:ea | aCollection includes: ea].
	cached do: [:ea | (aCollection includes: ea) ifFalse: [missing add: ea]].
	missing isEmpty ifFalse: [rescued at: aNode put: missing].
	^ missing asArray