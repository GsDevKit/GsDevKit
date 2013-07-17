class initialization
initialize
	self allSubclasses do: [:ea | ea new register]