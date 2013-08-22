building
buildOn: aBuilder
	^ aBuilder
		multipleSelectionColumnOS: self 
		with: [switch buildOn: aBuilder]