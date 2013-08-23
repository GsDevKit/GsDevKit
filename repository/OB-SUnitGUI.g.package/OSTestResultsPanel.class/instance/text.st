callbacks
text
	morph ~~ nil ifTrue: [ morph color: self statusColor ].
	^ self testSummary.