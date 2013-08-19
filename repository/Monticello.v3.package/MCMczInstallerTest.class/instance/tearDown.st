as yet unclassified
tearDown

	super tearDown.
	expected snapshot updatePackage: self mockPackage.
	self deleteFile.