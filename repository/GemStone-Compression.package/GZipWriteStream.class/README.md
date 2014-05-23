SwazooBuffer is used for efficient buffering of receiving or sending data to TCP socket. Efficiency is achieved with reusing of stream content array instead of initializing it everytime buffer is emptied, as was in previous Swazoo versions. 

SwazooBuffer is a subclass of ReadWriteStream, with additional #clear to empty buffer.

