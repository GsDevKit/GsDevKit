requests
timeStamp
	^ self method ifNotNilDo: [:method | 
		method inClass stampForMethod: method selector]