private
defaultSource
	^ String streamContents: [:stream |
		
		self writeMethodHeaderTo: stream.
		stream
			cr; tab;
			nextPutAll: '"This method does not exist."';
			cr; tab;
			nextPutAll: 'self halt.'
		]