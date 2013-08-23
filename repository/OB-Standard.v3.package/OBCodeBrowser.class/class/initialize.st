configuration
initialize
	"OBCodeBrowser initialize"
	Smalltalk 
		at: #Preferences 
		ifPresent: [:cl |
			cl
				addPreference: #chasingBrowsers
				categories: #('browsing')
				default: true
				balloonHelp: 'Use recursive multi-column browsers for senders, implementors, etc.']