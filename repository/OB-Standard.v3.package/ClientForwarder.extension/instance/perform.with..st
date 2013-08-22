*ob-standard-Forwarding
perform: aSelectorSymbol with: anObject

  ^ ClientForwarderSend new
      receiver: self
           clientObj: clientObject
           selector: aSelectorSymbol
           args: { anObject } ;
      defaultAction  "return error direct to GCI"