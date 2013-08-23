action
execute

	GsNMethod clearAllBreaks.
	OTRemoteDebugger sendClearAllBreaksSignal.
	self update.