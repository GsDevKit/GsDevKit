evaluating
value
	"Answer the result of evaluating the elements of
	the receiver."
	| answer |
	self
		do: [:each | answer := each value].
	^ answer