tokenizing
nextTag
	| tagName attributes nextChar namespaces |
	(self peek = $/)
		ifTrue: [^self nextEndTag].
	tagName _ self nextName.
	self skipSeparators.
	attributes _ Dictionary new: 33.
	namespaces _ Dictionary new: 5.
	[(nextChar _ self peek) == $> or: [nextChar == $/]] whileFalse: [
		self checkAndExpandReference: #content.
		self nextAttributeInto: attributes namespaces: namespaces.
		self skipSeparators.].
	self handleStartTag: tagName attributes: attributes namespaces: namespaces.
	self next == $/
		ifTrue: [
			self handleEndTag: tagName.
			self next].
	