OmniBrowser
oopInfoFor: aCollection

	| res |
	res := Array new.
	aCollection do: [:ea | 
		(ea isString or: [ea isSymbol])
			ifTrue: [ res add: ea asString; add: ea class name]
			ifFalse: [
				self addToPureExportSet: ea.
				res add: ea asOop printString.
				res add: ea proxyClassName asString]].
	^self stringFromCollection: res