*bootstrap
baseOverridesBootstrapSessionMethodsMetaclass: known
	self
		addTo: known
		forBehavior: Metaclass
		overrides: {
			#'theNonMetaClass' -> 'theNonMetaClass
  "Sent to a class or metaclass, always return the class.
   Used by topaz and Monticello."

  ^ thisClass
'.
	}