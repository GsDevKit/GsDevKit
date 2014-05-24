*gemstone-compression
byteAt: index put: newElement
  "Considers the receiver as an Array of bytes and sets the byte at position
 index to newElement.  The argument newElement replaces any previously stored
 value and must be an Integer between 0 and 255."

  ^ self _basicAt: index put: newElement