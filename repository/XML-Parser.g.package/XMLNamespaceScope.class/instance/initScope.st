private
initScope
	scope := OrderedCollection new: 20.
	currentBindings := Dictionary new.
	scope addLast: (Array with: 'http://www.w3.org/TR/REC-xml-names' with: currentBindings with: nil)