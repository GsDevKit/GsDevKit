instance creation
read: path type: type
	 " type: #binary, #text, #serverBinary, #serverText, #clientBinary, #clientText"

	^(self basicNew) openForRead: path type: type