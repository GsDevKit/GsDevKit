handling tokens
handleStartTag: tagName attributes: attributes
	self log: 'Start tag: ' , tagName.
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]