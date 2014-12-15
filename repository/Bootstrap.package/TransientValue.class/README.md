I keep my references alive for the duration of the Session. Being dbTransient means that I may be committed, but my value will not be committed.

I am useful for wrapping instances of Semaphore, GsSocket, etc. from objects that are persistent themselves. The application must arrange to refresh the value from within a new session, but once the value has been set, I guarantee that the value will not be persisted and will not be garbage collected.

Unlike TransientStackValue, there is no need to ensure that I am reachable from SessionTemps as I maintain my own reference from SessionTemps.