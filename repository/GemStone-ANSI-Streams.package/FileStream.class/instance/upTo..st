Accessing
upTo: anObject

"Returns all objects up to the given value or the end of the stream."

| result |
result := self collectionSpecies new.
[self atEnd not _and: [self peek ~= anObject]] whileTrue: [
  result add: self next
].
self atEnd not ifTrue: [
  self next
].
^result