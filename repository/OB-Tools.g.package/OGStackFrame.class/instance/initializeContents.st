initialization
initializeContents
	|  offsets i |
	process stackDepth == nil ifTrue: [ ^self initializeNonRunningContents ].
	breakpointOnStack := previousStackFrame == nil
		ifTrue: [ false ]
		ifFalse: [ previousStackFrame isGsReturnNothingEnableEvents ].
	frame := process _frameContentsAt: index.
	label := (process class  _reportWithFrameContents: frame).
	gsMethod := frame at: 1.
	ipOffset := frame at: 2.
	stepPoint := self stepPointForIp: ipOffset.
	offsets := gsMethod _sourceOffsets.
	offset := offsets at: (stepPoint min: offsets size).
	frameSelf := frame at: 8.
	variables := frame at: 9.
	receiver := frame at: 10.
