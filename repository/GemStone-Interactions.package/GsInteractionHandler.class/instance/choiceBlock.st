accessing
choiceBlock
  choiceBlock ifNil: [ ^ self defaultBlock ].
  ^ choiceBlock