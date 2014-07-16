*monticellogs
authorInitials
  ^ SessionTemps current
    at: GsPackagePolicy authorInitialsGlobalName
    ifAbsent: [ 
      (GsSession currentSession objectNamed: #'UserGlobals')
        at: #'GS_tODE_AuthorInitials'
        ifAbsent: [ GsPackagePolicy current authorInitials ] ]