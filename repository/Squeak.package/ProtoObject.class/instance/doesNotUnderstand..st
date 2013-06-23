error handling
doesNotUnderstand: aMessageDescriptor

"Generates an error reporting that the receiver cannot respond to a message
 because no compiled method was found for the selector.  The argument
 aMessageDescriptor is a two-element Array.  The first element is the selector
 that was not found and the second is an Array of arguments for the message."

| selector argList |

selector := aMessageDescriptor at: 1 .  "put selector into temporary for easier"
				        "  debugging with topaz."
argList := #[ self, selector, (aMessageDescriptor at: 2) ].

System signal: 2010 "#rtErrDoesNotUnderstand, hard-coded for speed"
       args: argList signalDictionary: GemStoneError .