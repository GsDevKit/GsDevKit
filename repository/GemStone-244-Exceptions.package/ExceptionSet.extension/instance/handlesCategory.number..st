*gemstone-244-exceptions
handlesCategory: anObject number: anInteger
	"fix for Issue 129"

	^selectors anySatisfy: [:each | each handlesCategory: anObject number: anInteger].
