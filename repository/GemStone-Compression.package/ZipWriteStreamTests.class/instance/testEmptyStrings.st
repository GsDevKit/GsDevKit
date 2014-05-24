tests
testEmptyStrings
  | aStream zipStream x |
  aStream := ByteArray new writeStream binary.
  zipStream := ZipWriteStream on: aStream.
  zipStream nextStringPut: ''.
  zipStream nextStringPut: ''.
  zipStream close.
  aStream close.
  aStream := aStream contents readStream binary.
  zipStream := ZipReadStream on: aStream.
  self assert: (x := zipStream nextString) isEmpty.
  self assert: zipStream nextString isEmpty.
  zipStream close.
  aStream close