Accessing
nextLine

| result cr lf char |
result := String new.
cr := Character cr.
lf := Character lf.
[self atEnd not and: [(char := self peek asCharacter) ~~ cr _and: [char ~~ lf]]] whileTrue: [
  result add: self next asCharacter
].
self atEnd not ifTrue: [
  self next.
  (self atEnd not and: [char == cr and: [self peek asCharacter == lf]]) ifTrue: [
    self next.
  ].
].

^result
