private
putAsXMLString: aValue
	self stream nextPut: $".
	self pcData: aValue.
	self stream nextPut: $"