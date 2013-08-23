accessing
reference
    | references |
    "this is a bit hacky actually..."
    references := SystemNavigation default allImplementorsOf: selector.
    references ifEmpty: [self error: 'No method with this selector exists!'].
    ^references first