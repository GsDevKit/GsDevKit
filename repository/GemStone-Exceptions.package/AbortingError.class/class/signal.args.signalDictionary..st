ANSI - exceptionSignaler
signal: anInteger args: anArray signalDictionary: anErrorDict
	"InterpreterErrors are always use the GemStoneError dictionary"
	
	self shouldNotImplement: #signal:args:signalDictionary:
