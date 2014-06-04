interaction
interactWithExplore: aGsInspectInteraction
  Smalltalk
    at: #'OTChasingInspector'
    ifPresent: [ :cls | ^ (cls on: aGsInspectInteraction theObject) open ].
  ^ aGsInspectInteraction theObject