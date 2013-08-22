*ob-standard-Forwarding
perform: aSelectorSymbol withArguments: anArray
	"A special case for the forwarder - avoid having to unwrap things on the client" 
	^System _signalGciError: 2336 "err clientForwarderSend hard coded for speed"
    		args:#[ self, clientObject "receiver" , 
	    		aSelectorSymbol "selector" , 
          	anArray "arguments to selector" ,
			anArray size "number of arguments" ]
    		signalDictionary: GemStoneError