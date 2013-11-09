interaction
interactWithInspect: aGsInspectInteraction
  Smalltalk
    at: #'OTInspector'
    ifPresent: [ :cls | ^ (cls on: aGsInspectInteraction theObject) open ].
  ^ aGsInspectInteraction theObject