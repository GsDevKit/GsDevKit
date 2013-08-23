initialization
setRawQuery: aString panel: aPanel 
	self setRawQuery: aString.
	browser := aPanel browser.
	^ self