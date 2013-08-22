notification
handleDirectoryRequest: anMCDirectoryRequest

	^(self clientForwarderMap at:#directory) signalDirectoryRequestUsing: anMCDirectoryRequest