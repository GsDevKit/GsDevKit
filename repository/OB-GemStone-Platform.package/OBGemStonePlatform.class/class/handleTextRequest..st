notification
handleTextRequest: anOBTextRequest

	^(self clientForwarderMap at:#text ifAbsent: [ ^anOBTextRequest template ]) signalTextRequestUsing: anOBTextRequest