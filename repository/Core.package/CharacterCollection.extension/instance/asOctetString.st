*core-squeak
asOctetString

	| res |
	res := self _asString .
	res == nil ifTrue:[ ^ self ].
	^ res
