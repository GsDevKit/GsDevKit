*squeak
reversed
	"Answer a copy of the receiver with element order reversed."

	| n result src |
	n _ self size.
	"GemStone disallows at: put: on a SortedCollection ... reversing a SortedCollection without changing the
	sortBlock seems odd"
	result _ OrderedCollection new: n.
	src _ n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src _ src - 1))].
	^ result