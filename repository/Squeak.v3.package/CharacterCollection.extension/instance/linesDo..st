*squeak
linesDo: aBlock

	| input crlf cr lf |
	input := self contents readStream.
	crlf := String crlf.
	cr := Character cr.
	lf := Character lf.
	[ input atEnd not ] whileTrue: [ | res |
		res := String new.
		[ input atEnd not and: [ (crlf includesIdentical: input peek) not ]] whileTrue: [
			res add: input next ].
		aBlock value: res.
		input atEnd not ifTrue: [ 
				input next = Character cr ifTrue: [ 
					(input atEnd not and: [ input peek = Character lf]) ifTrue: [ input next ]]]].
