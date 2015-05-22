*squeak-converting
asInteger

	| start stream |
	start := self findFirst: [:char | char isDigit].
	start isZero ifTrue: [^ nil].
	stream := self readStream position: start - 1.
	^ Integer readFrom: stream