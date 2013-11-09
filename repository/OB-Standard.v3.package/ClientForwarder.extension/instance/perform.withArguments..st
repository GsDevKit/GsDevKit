*ob-standard-Forwarding
perform: aSelectorSymbol withArguments: anArray
  "A special case for the forwarder - avoid having to unwrap things on the client" 

  ^ ClientForwarderSend new
      receiver: self
           clientObj: clientObject
           selector: aSelectorSymbol
           args: anArray ;
      defaultAction  "return error direct to GCI"
