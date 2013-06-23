*gemstone-23x-exceptions
handlesCategory: anObject number: anInteger
	"This method may be overridden for ExceptionSelectors that are more discriminating.
	For example, one might want to trap all GemStoneError numbers in a certain range."

	^selectors anySatisfy: [:each | each handlesCategory: anObject number: anInteger].
