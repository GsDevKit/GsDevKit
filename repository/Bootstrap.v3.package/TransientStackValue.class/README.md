I keep my references alive as long as I am referenced from the stack (or other temp object). Being dbTransient means that I may be committed, but my value will not be committed.

I am useful for wrapping instances of Semaphore, GsSocket, etc. from objects that pretty much only live on the stack, but may be persisted in continuation that are saved for debugging purposes.

If I or my value are not directly referenced from SessionTemps in some way, then I and my value is subject to garbage collection ...

If you cannot guaranteed that I am reachable from SessionTemps, then use TransientValue. TransientValue guarantees a SessionTemps reference.