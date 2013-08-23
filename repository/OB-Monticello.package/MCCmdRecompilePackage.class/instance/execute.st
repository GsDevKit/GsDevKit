execution
execute
	target workingCopy package packageInfo methods do: [:ea | ea actualClass recompile: ea methodSymbol].
	self doAutoCommit