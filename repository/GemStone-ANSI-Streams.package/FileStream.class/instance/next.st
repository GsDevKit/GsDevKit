accessing
next

	| res |
	(res := self next: 1) ifNil: [ ^nil ].
	^res at: 1