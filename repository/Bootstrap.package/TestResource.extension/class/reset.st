*bootstrap
reset

	self current ~~ nil ifTrue: [
		[self current tearDown] ensure: [
			self current: nil]]
