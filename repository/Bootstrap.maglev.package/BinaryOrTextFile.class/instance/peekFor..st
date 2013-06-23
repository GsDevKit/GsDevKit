Accessing
peekFor: anObject

self peek = anObject ifTrue: [
  self next.
  ^true.
].
^false.
