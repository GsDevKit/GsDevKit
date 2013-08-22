limit class
limitClass: aClass 
	requestor 
		announce: (OBNodeChanged node: aClass);
		announce: (OBRefreshRequired new)