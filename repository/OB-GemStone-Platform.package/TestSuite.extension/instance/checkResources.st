*ob-gemstone-platform-GSExtensions
checkResources
	self resources do: [ :each |
		each isAvailable
			ifFalse: [ TestResult signalErrorWith: 'Resource ' , each name , ' could not be initialized' ] ].