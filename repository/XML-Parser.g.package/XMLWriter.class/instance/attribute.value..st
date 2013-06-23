writing xml
attribute: attributeName value: attributeValue
	self stream
		space;
		nextPutAll: attributeName.
	self
		eq;
		putAsXMLString: attributeValue