private
connectTo: portNumber on: aHost
  "Connect the receiver to the server socket identified by portNumber and aHost. 
 aHost may be the name of the host or its address, 
 or aHost == -1 for <broadcase> , or aHost == nil for IN6ADDR_ANY_INIT .
 portNumber maybe either a SmallInteger, or the String name of a service.
 Returns true if the connection succeeded and false if not."

  ^ self connectTo: portNumber on: aHost timeoutMs: -1