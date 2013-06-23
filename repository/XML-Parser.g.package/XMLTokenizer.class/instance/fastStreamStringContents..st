private
fastStreamStringContents: writeStream
	| newSize |
	newSize _ writeStream position - 1.
	^(String new: newSize)
		replaceFrom: 1
		to: newSize
		with: writeStream originalContents
		startingAt: 1