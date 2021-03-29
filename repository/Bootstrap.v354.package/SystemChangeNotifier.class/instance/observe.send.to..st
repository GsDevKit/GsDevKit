subscription
observe: aClass send: aSelector to: anObject
	self
		observe: aClass
		do: (MessageSend receiver: anObject selector: aSelector)