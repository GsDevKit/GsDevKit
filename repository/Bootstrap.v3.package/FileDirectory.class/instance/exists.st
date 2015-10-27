testing
exists
  | result |
  result := GsFile _exists: self pathString onClient: self onClient.
  result ifNil: [ self error: 'Disk error: ' , GsFile lastErrorString ].
  ^ result