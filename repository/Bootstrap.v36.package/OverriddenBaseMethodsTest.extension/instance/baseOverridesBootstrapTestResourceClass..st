*bootstrap
baseOverridesBootstrapTestResourceClass: known
	self
		addTo: known
		forBehavior: TestResource class
		overrides: {
			#'reset' -> 'reset

	current ~~ nil ifTrue: [
		[current tearDown] ensure: [
			current := nil]]
'.
	}