*ob-standard-Forwarding
perform: aSelectorSymbol with: firstObject with: secondObject
	"A special case for the forwarder - avoid having to unwrap things on the client" 
	^ self perform: aSelectorSymbol withArguments: #[firstObject, secondObject]