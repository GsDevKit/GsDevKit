enumerating
elementsDo: aBlock
	elements
		ifNotNil: [
			self elements do: [:each | aBlock value: each]]