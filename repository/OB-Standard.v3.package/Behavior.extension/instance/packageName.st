*ob-standard
packageName
    ^self package ifNotNilDo: [:pkg | pkg packageName] ifNil: ['']