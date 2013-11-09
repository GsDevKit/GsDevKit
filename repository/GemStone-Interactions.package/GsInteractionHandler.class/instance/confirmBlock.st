accessing
confirmBlock
  confirmBlock ifNil: [ ^ self defaultBlock ].
  ^ confirmBlock