execution
clientExecute
	"Wrapper method used so that all client #execute calls can be wrapped by an OBUpdateConcentrator"

	OBUpdateConcentrator default 
		recordUpdatesDuring: [ ^self execute ]