private
establishLocalBindings
	(self currentScope at: 2)
		ifNil: [
			currentBindings := currentBindings copy.
			self currentScope at: 2 put: currentBindings]