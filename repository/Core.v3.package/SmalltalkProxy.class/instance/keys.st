testing
keys
  | symList keys |
  symList := System myUserProfile symbolList.
  keys := IdentitySet new.
  1 to: symList size do: [ :j | (symList at: j) ifNotNil: [ :aDict | keys addAll: aDict keys ] ].
  ^ keys