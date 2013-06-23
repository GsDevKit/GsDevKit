private
createHistoryFor: aSelector in: aBehavior

	| meth |
	meth := aBehavior compiledMethodAt: aSelector ifAbsent: [ ^Dictionary new ].
	self addVersionFor: meth.
	^self dictionary at: aBehavior