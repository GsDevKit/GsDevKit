filtering
switchFilter
	^ filters 
		detect: [:ea | ea wantsSwitch]
		ifNone: [nil]