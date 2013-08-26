running
setUp
	super setUp.
	expected := self mockSnapshot.
	stream := RWBinaryOrTextStream on: String new.