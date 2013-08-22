notification
handleCloseRequest: anOBBrowseRequest

	^(self clientForwarderMap at:#close) signalCloseRequestUsing: anOBBrowseRequest