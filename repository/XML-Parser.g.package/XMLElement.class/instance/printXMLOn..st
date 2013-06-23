printing
printXMLOn: writer
	writer startElement: self name attributeList: self attributes.
	(writer canonical not
		and: [self isEmpty and: [self attributes isEmpty not]])
		ifTrue: [writer endEmptyTag: self name]
		ifFalse: [
			writer endTag.
			self contentsDo: [:content | content printXMLOn: writer].
			super printXMLOn: writer.
			writer endTag: self name]