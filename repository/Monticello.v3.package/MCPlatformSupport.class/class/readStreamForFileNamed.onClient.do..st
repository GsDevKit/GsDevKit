io
readStreamForFileNamed: aString onClient: onClient do: aBlock
  | file stream |
  file := GsFile open: aString mode: 'rb' onClient: onClient.
  file ifNil:[ self error:' file not found, ' , GsFile lastErrorString].
  stream := RWBinaryOrTextStream on: file contents.
  stream name: aString.
  file close.
  ^aBlock value: stream
