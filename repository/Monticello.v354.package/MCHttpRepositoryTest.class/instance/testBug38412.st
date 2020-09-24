tests
testBug38412
	| proxyServer proxyPort |
	(proxyServer := HTTPSocket httpProxyServer) notNil 
		ifTrue: [ proxyPort := HTTPSocket httpProxyPort ].
	[
	HTTPSocket useProxyServerNamed: nil port: nil. 
	self 
		should: [
			(MCHttpRepository
				location: 'http://dnu.example.com/ss/monticello'
				user: ''
				password: '') allFileNames ]
		raise: Error] 
			ensure: [ HTTPSocket useProxyServerNamed: proxyServer port: proxyPort ]
