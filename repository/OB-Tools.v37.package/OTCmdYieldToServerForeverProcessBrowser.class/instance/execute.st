execution
execute
	
	| proc |
	(self confirm: 'This command will not return and will have to be interrupted to return control to the client.')
		ifFalse: [ ^self ].
	(proc := self class currentProcess) ~~ nil
		ifTrue: [
			self class currentProcess: nil.
			proc terminate].
	self class currentProcess: Processor activeProcess.
	[true] whileTrue: [ (Delay forSeconds: 10) wait ].
