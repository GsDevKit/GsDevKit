enumerating
contentsDo: aBlock
	contents
		ifNotNil: [
			self contents do: [:each | aBlock value: each]]