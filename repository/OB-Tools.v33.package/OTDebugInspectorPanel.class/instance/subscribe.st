updating
subscribe
	receiverInspector := OTDebugInspector new.
	contextInspector := OTDebugInspector new.
	self announcer 
		observe: OBSelectionChanged
		send: #selectionChanged:
		to: self