*packageinfo-base
gather: aBlock
  | res |
  res := { } .
  1 to: self size do: [:j | res addAll: (aBlock value: (self at:j) ) ].
  ^ res
