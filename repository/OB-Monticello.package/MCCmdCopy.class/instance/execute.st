execution
execute
	self pickRepository ifNotNilDo:
		[:ea |
		ea storeVersion: target version.
		self doAutoCommit]