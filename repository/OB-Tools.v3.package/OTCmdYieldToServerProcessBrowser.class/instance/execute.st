execution
execute
	| proc |
	self update.
	(proc := self class currentProcess) ~~ nil
		ifTrue: [
			self class currentProcess: nil.
			proc terminate].
	self class currentProcess: Processor activeProcess.
	(Delay forSeconds: 5) wait.
	self update