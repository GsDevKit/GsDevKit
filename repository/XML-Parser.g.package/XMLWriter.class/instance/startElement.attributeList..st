writing xml
startElement: elementName attributeList: attributeList
	self canonical
		ifFalse: [self stream cr].
	self startTag: elementName.
	attributeList keys asSortedCollection do: [:key |
		self attribute: key value: (attributeList at: key)]