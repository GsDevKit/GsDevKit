action
execute

	GsMethod clearAllBreaks.
	OTRemoteDebugger sendClearAllBreaksSignal.
	self update.