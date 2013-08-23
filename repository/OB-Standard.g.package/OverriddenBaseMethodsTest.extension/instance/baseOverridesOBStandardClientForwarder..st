*ob-standard
baseOverridesOBStandardClientForwarder: known
	self
		addTo: known
		forBehavior: ClientForwarder
		overrides: {
			#'doesNotUnderstand:' -> 'doesNotUnderstand: aMessageDescriptor

"Sends the message described by aMessageDescriptor to the client object
 represented by the value of the receivers'' instance variable clientObject.
 Returns the result from executing the forwarded message in the client."

"Implemented by raising the error #clientForwarderSend to the client.
 This error will be returned to the client as the result of the GemBuilder for C
 call which caused the GemStone Smalltalk execution containing the forwarded
 message send.

 Upon completion of the method invoked by the forwarded message, the
 client should call GciContinueWith(anOop), where anOop is the GemStone
 replicate of the result of the client message send."

^ System _signalGciError: 2336 "err clientForwarderSend hard coded for speed"
    args:#[ self, clientObject "receiver" , 
	    (aMessageDescriptor at: 1) "selector" , 
            (aMessageDescriptor at: 2) "arguments to selector" 
          ]
    signalDictionary: GemStoneError
'.
	}