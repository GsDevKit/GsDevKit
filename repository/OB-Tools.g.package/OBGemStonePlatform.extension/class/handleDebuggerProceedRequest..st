*ob-tools
handleDebuggerProceedRequest: anOGDebuggerProceedRequest

	^(self clientForwarderMap at:#debugger) signalDebuggerProceedRequestUsing: anOGDebuggerProceedRequest