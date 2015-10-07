*ob-tools
baseOverridesOBToolsWarning: known

	self 
		addTo: known 
		forBehavior: Warning 
		overrides: {
			#defaultAction -> 'defaultAction
	"The user should be notified of the occurrence of an exceptional
	occurrence and given an option of continuing or aborting the
	computation. The description of the occurrence should include
	any text specified as the argument of the #signal: message.

	It is not so clear how this would be done in GemStone."

	^super defaultAction.
'.
	}