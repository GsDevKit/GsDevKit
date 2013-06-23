private
log: aInfo 
	"maybe we need an extra commit, if we are not using seaside ?"
	"(UserGlobals at: #log) add: aInfo"
	(ObjectLogEntry warn: 'Could not send mail to: ', self to, '. Reason: ', aInfo) addToLog.