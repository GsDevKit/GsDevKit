class initialization
resetSystemOrganization
  "self resetSystemOrganization"

  | ar |
  ar := (GsSession currentSession symbolList dictionariesAndSymbolsOf: self)
    at: 1.
  (ar at: 1) at: #'SystemOrganization' put: self new