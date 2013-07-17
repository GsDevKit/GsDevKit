*packageinfo-base
gather: aBlock
  | res |
  res := { } .
  self do: [:ea | res addAll: (aBlock value: ea)].
  ^ res
