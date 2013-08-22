registry protocol
openBrowserView: adaptor label: ignored 
	^ self browserClass 
		openOnClass: adaptor targetClass 
		selector: adaptor selector