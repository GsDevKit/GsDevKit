accessing
finalBlockHome
  (self receiver isKindOf: ExecBlock)
    ifFalse: [ ^ self ].
  ^ self sender finalBlockHomeFor: self