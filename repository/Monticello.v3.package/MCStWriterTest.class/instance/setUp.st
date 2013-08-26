testing
setUp
	super setUp.
	stream := RWBinaryOrTextStream on: String new.
	writer := MCStWriter on: stream.
