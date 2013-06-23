private
pop
	| oldTop |
	oldTop _ self stack removeLast.
	entity _ oldTop.
	^oldTop