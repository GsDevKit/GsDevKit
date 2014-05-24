private
rewindData
	super rewindData.
	(stream isNil or: [ stream closed ])
		ifTrue: [ self error: 'stream missing or closed' ].
	stream position: (localHeaderRelativeOffset + 4 + 1).
	self skipLocalDirectoryFileHeaderFrom: stream.