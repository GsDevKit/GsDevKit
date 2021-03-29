actions
removeVersionsForMethod: aGsMethod

  | cls selector mDict |
  cls := aGsMethod inClass.
  selector := aGsMethod selector.
  mDict := self dictionary at: cls ifAbsent: [ ^self ].
  mDict removeKey: selector ifAbsent: [ ].