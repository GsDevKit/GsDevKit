notification
handleBrowseRequest: anOBBrowseRequest

	^(self clientForwarderMap at:#browse) signalBrowseRequestUsing: anOBBrowseRequest