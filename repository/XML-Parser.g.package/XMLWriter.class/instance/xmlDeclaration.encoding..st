writing xml
xmlDeclaration: versionString encoding: encodingString
	self canonical
		ifFalse: [
			self
				startPI: 'xml';
				attribute: 'version' value: versionString;
				attribute: 'encoding' value: encodingString;
				endPI.
			self stream flush]