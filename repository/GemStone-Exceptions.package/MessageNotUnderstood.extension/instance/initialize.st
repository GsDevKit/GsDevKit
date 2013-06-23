*gemstone-exceptions
initialize

	"if MessageNotUnderstood is signaled directly, then the receiver, message and args are undefined, but GemStone needs reasonable defaults"

	gsCategory 	:= GemStoneError.
	gsNumber 		:= 2010.
	gsArguments 	:= #( nil nil #()).
