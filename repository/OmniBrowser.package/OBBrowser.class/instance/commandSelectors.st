initializing
commandSelectors
	| all obsolete commands |
	all _ self class allSelectors.
	obsolete _ Set new.
	all do: [:ea | ((ea beginsWith: 'obsolete') and: [ea isUnary] ) ifTrue: [obsolete add: (self perform: ea)]].
	commands _ all select: [:ea | ea beginsWith: 'cmd'].
	^ commands reject: [:ea | obsolete includes: ea]