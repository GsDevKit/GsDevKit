*core31x-squeak
asString
  "Convert to a String with Characters for each byte"

  ^ String withBytes: (self copy changeClassTo: ByteArray)