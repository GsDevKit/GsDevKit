accessing
type
  | ty |
  (ty := type) == #transient ifTrue:[
     ty := { #normal . #( #dbTransient ) }.
     type := ty .
  ].
  ^ ty
