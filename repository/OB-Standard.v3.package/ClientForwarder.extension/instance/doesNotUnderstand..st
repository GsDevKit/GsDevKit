*ob-standard-Forwarding
doesNotUnderstand: aMessageDescriptor

"Sends the message described by aMessageDescriptor to the client object
 represented by the value of the receivers' instance variable clientObject.
 Returns the result from executing the forwarded message in the client."

"Implemented by raising the error #clientForwarderSend to the client.
 This error will be returned to the client as the result of the GemBuilder for C
 call which caused the GemStone Smalltalk execution containing the forwarded
 message send.

 Upon completion of the method invoked by the forwarded message, the
 client should call GciContinueWith(anOop), where anOop is the GemStone
 replicate of the result of the client message send."

^ self perform: (aMessageDescriptor at: 1)  withArguments: (aMessageDescriptor at: 2)