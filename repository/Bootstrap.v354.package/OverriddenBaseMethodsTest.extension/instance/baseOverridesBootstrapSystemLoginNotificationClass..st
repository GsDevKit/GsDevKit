*bootstrap
baseOverridesBootstrapSystemLoginNotificationClass: known
	self
		addTo: known
		forBehavior: SystemLoginNotification class
		overrides: {
			#'sessionStart' -> 'sessionStart
    "Nothing by default"
'.
	}