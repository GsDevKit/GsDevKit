initializing
commandSelectors
	| all obsolete commands |
	all := self class selectors.
	obsolete := Set new.
	all do: [:ea | (ea beginsWith: 'obsolete') ifTrue: [obsolete add: (self perform: ea)]].
	commands := all select: [:ea | ea beginsWith: 'cmd'].
	^ commands reject: [:ea | obsolete includes: ea]