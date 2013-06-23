*core-squeak
normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen new |
	self isSpecial ifTrue: [ "SmallInteger" ^self ]. 
	"First establish len = significant length"
	len _ oldLen _ self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len _ len - 1].

	"Now check if in SmallInteger range"
	sLen _ SmallInteger maxVal digitLength.
	(len <= sLen
		and: [(self digitAt: sLen) <= (SmallInteger maxVal digitAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val _ 0.
				len to: 1 by: -1 do:
					[:i | val _ (val *256) + (self digitAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen ifFalse: [^ self].
	"in-line growTo:"
	new := self class _new: len neg: self negative.
	1 to: len do: [:i | new digitAt: i put: (self digitAt: i) ].
	^new