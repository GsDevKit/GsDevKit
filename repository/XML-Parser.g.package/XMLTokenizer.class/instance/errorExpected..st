errors
errorExpected: expectedString
	| actualString |
	actualString := ''.
	self atEnd
		ifFalse: [
			[actualString := self next: 20]
				on: Error
				do: [:ex | ]].
	self parseError: 'XML expected ' , expectedString printString , ': ' , actualString