Accessing
skipSeparators
"Skip any objects immediately next in the stream that respond true to
 isSeparator."

[self atEnd not _and: [self peek isSeparator]] whileTrue: [ self next ]