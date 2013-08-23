notification
handleInformRequest: anOBInformRequest

	^(self clientForwarderMap at:#inform ifAbsent: [ ^true ] ) signalInformRequestUsing: anOBInformRequest