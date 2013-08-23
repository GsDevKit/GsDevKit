as yet unclassified
setRawQuery: aString panel: aPanel
	pattern := aString findTokens: #*.
	isWild := aString includes: $*.
	browser := aPanel browser.
	^ self