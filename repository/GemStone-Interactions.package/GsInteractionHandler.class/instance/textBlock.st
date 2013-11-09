accessing
textBlock
  textBlock ifNil: [ ^ self defaultBlock ].
  ^ textBlock