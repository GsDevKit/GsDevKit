initialization
openForRead: fileName type: type
	 " type: #binary, #text, #serverBinary, #serverText, #clientBinary, #clientText"

	| mode onClient path |
	path := self fullName: fileName type: type.
	streamType := #text.
	(type == #binary or: [ type == #serverBinary ])
		ifTrue: [ mode := 'r+b'. onClient := false. streamType := #binary ].
	(type == #text or: [ type == #serverText ] )
		ifTrue: [ mode := 'r+'. onClient := false ].
	type == #clientBinary
		ifTrue: [ mode := 'r+b'. onClient := true. streamType := #binary ].
	type == #clientText
		ifTrue: [ mode := 'r+'. onClient := true ].
	gsfile := GsFile open: path mode: mode onClient: onClient.
	gsfile == nil ifTrue: [ ^self error: 'Error opening file: ', (GsFile classUserAction: #GsfClassError onClient: onClient with: nil) ].