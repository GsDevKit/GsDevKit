accessing
multiLineTextBlock
  multiLineTextBlock ifNil: [ ^ self defaultBlock ].
  ^ multiLineTextBlock