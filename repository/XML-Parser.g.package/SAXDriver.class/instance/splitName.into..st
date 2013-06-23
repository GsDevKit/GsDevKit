namespaces
splitName: aName into: twoArgsBlock
	"Split the name into namespace and local name (the block arguments).
	Handle both qualified and unqualified names using the default name space"

	| i ns ln |
	i _ aName lastIndexOf: $:.
	i = 0
		ifTrue: [
			ns := nil.
			ln := aName]
		ifFalse: [
			ns := aName copyFrom: 1 to: (i - 1).
			ln := aName copyFrom: i+1 to: aName size].
	twoArgsBlock value: ns value: ln