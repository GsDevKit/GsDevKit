*core-squeak
valueWithPossibleArgs: anArray

	| n arrSiz |
	(n := self numArgs) == 0 ifTrue: [^ self value].
	n == (arrSiz := anArray size) ifTrue: [^ self valueWithArguments: anArray].
	arrSiz < n 
		ifTrue: [ ^self valueWithArguments: ((Array new: n) replaceFrom: 1 to: arrSiz with: anArray startingAt: 1) ].
	^ self valueWithArguments: (anArray copyFrom: 1 to: (n > arrSiz ifTrue:[ arrSiz ] ifFalse:[ n ]))
