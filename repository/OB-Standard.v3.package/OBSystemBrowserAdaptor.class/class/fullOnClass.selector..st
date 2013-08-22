registry protocol
fullOnClass: aClass selector: aSelector
	^ self browserClass 
		openOnClass: aClass
		selector: aSelector