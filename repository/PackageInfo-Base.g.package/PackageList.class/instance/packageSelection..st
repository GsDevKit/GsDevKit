morphic
packageSelection: aNumber
	selectedPackage _ self packages at: aNumber ifAbsent: [].
	self changed: #packageSelection