accessing
result

	result == nil ifTrue: [ result := self browser root result ].
	^result