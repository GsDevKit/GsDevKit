*ob-standard-Forwarding
perform: aSelectorSymbol
	"A special case for the forwarder - avoid having to unwrap things on the client" 
	^ self perform: aSelectorSymbol withArguments: #()