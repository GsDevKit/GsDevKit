updating
aboutToChangeSilently: ann
	self canDiscardEdits ifFalse: [ann veto]