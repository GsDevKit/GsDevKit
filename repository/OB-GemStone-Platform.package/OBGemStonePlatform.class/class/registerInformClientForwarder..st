notification
registerInformClientForwarder: aClientForwarder
  self registerInteractionRequest.
  self clientForwarderMap at: #'inform' put: aClientForwarder