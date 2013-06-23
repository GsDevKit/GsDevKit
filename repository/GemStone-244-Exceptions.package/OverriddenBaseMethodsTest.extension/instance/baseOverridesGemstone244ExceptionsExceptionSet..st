*gemstone-244-exceptions
baseOverridesGemstone244ExceptionsExceptionSet: known
	self
		addTo: known
		forBehavior: ExceptionSet
		overrides: {
			#'handlesCategory:number:' -> 'handlesCategory: anObject number: anInteger
	"This method may be overridden for ExceptionSelectors that are more discriminating.
	For example, one might want to trap all GemStoneError numbers in a certain range."

	^self handles: anObject.
'.
	}