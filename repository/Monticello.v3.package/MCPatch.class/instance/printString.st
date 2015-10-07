printing
printString
  | arr res lf |
  arr := Array new.
  self operations do: [ :op | arr add: op printString ].
  arr := arr sortAscending: ''.
  res := String new.
  lf := Character lf.
  arr
    do: [ :aStr | 
      res
        addAll: aStr;
        add: lf ].
  ^ res