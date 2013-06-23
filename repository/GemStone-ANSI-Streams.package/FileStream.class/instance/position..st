accessing
position: pos
	"Set the receiver's position as indicated."

	(pos > self size or: [ pos < 0 ]) ifTrue: [ ^self error: 'Attempt to position file beyond bounds of file' ].
	self gsfile position: pos