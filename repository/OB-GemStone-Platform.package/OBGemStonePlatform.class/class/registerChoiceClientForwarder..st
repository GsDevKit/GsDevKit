notification
registerChoiceClientForwarder: aClientForwarder
  self registerInteractionRequest.
  self clientForwarderMap at: #'choice' put: aClientForwarder