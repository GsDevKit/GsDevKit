*core322x
resetSystemOrganization
  "self resetSystemOrganization"

  | ar |
  ar := (System myUserProfile symbolList dictionariesAndSymbolsOf: self) at: 1.
  (ar at: 1) at: #'SystemOrganization' put: self new