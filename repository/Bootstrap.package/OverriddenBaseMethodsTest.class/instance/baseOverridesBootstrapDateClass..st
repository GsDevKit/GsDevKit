*bootstrap
baseOverridesBootstrapDateClass: known
	self
		addTo: known
		forBehavior: Date class
		overrides: {
			#'fromString:' -> 'fromString: aString

"Creates and returns an instance of the receiver from the String aString.
 The String expresses the date in the default format (DD/MM/YYYY).
 Generates an error if the String does not conform to the format."

^ self fromString: aString usingFormat: #(1 2 3 $/ 1 1)
'.
	}