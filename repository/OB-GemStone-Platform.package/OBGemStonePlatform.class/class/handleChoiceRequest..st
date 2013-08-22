notification
handleChoiceRequest: anOBChoiceRequest

	^(self clientForwarderMap at:#choice) signalChoiceRequestUsing: anOBChoiceRequest