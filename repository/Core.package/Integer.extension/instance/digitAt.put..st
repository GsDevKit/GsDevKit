*core-squeak
digitAt: n put: value 
	" Insert the value into the nth base 256 digit of the receiver. 
	  GemStone natively uses base 32678, do can't do a direct store."
	
	| int |
	value == 0 ifTrue: [ ^self ].
	int := value bitShift: (n-1)*8.
	1 to: int _digitLength do: [:i | | dig ndig |
		(ndig := int _digitAt: i) > 0
			ifTrue: [ 
				dig := self _digitAt: i.
				self _digitAt: i put: (dig + ndig) ]].