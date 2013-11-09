accessing
inspectBlock
  inspectBlock ifNil: [ ^ self defaultBlock ].
  ^ inspectBlock