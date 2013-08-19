as yet unclassified
flushAllCaches
	self default repositoriesDo:
		[ : each | each flushCache ]