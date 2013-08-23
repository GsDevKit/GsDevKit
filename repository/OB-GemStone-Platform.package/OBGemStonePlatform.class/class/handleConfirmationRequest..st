notification
handleConfirmationRequest: anOBConfirmationRequest

	^(self clientForwarderMap at:#confirmation ifAbsent: [ ^true ]) signalConfirmationRequestUsing: anOBConfirmationRequest