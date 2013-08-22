*ob-standard-Forwarding
perform: aSelectorSymbol with: anObject
	"Optimized, since changed: messages are heavy user of this" 
	^ System _signalGciError: 2336 "err clientForwarderSend hard coded for speed"
    		args:#[ self, clientObject "receiver" , 
	    		aSelectorSymbol "selector" , 
          		#[anObject] "arguments to selector",
			1 "number of arguments",
			anObject "first argument"
     		]
    	signalDictionary: GemStoneError