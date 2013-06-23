initialization
openForWrite: fileName mode: modeSymbol check: check type: type
	"mode: #create, #append, #truncate
	 check: 
	  mode == #create and check == false and the file exists, then the file is used
	  mode == #create and check == true and the file exists, an error is thrown
	  mode == #append and check == false and the file does not exist then it is created
	  mode == #append and check == true and the file does not exist an error is thrown
	  mode == #truncate and check == false and file does not exist then it is created
	  mode == #truncate and check == true and the file does not exist an error is thrown
	 type: #binary, #text, #serverBinary, #serverText, #clientBinary, #clientText
	"
	
	| prefix mode onClient path |
	path := self fullName: fileName type: type.
	prefix := 'w+'.
	streamType := #text.
	modeSymbol == #append
		ifTrue: [ prefix := 'a+' ].
	(type == #binary or: [ type == #serverBinary ])
		ifTrue: [ mode := prefix, 'b'. onClient := false. streamType := #binary ].
	(type == #text or: [ type == #serverText ] )
		ifTrue: [ mode := prefix. onClient := false ].
	type == #clientBinary
		ifTrue: [ mode := prefix, 'b'. onClient := true. streamType := #binary ].
	type == #clientText
		ifTrue: [ mode := prefix. onClient := true ].
	(modeSymbol == #create and: [ check and: [ GsFile _exists: path onClient: onClient ]])
		ifTrue: [ ^self error: 'The file already exists.' ].
	((modeSymbol == #truncate or: [ modeSymbol == #append]) and: [ check and: [ (GsFile _exists: path onClient: onClient) not ]])
		ifTrue: [ ^self error: 'The file does not exist.' ].
	gsfile := GsFile open: path mode: mode onClient: onClient.
	modeSymbol == #truncate
		ifTrue: [ self truncate: 0 ].
	modeSymbol == #append
		ifTrue: [ gsfile seekFromEnd: 0 ].
