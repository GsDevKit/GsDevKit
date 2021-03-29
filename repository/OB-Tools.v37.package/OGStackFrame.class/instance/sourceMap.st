accessing
sourceMap
	"Take the value returned by #pc, subtract 1 if not at the top, 
	then use as an index into this collection. The value is an 
	Interval to highlight. The top frame is special because it
	highlights the next message to send, while all other frames
	highlight the message already sent.
	
	To get the Interval, we take the ip, and using methods in 
	GsMethod convert the ip to a step point. Once we have a 
	step point, we can convert it to a source offset.
	
	If pc returns 1, then the first element is used for every stack
	frame. If pc returns 2, then the top stack frame uses #2 and
	all the remaining use #1."

	| end |
	end := self stepPointEnd.
	^SortedCollection 
		with: 1 -> (offset to: end) 
		with: 2 -> (offset to: end).