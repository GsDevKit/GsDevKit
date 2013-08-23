commands
cmdDebug
	^ OTCmdDebugger allSubclasses select: [:ea | ea isForRemoteDebugger ]