accessing
informBlock
  informBlock ifNil: [ ^ self defaultBlock ].
  ^ informBlock