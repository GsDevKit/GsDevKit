Positioning
atEnd
  ^ super atEnd ifNil: [ self error: self lastErrorString ]