*core
baseOverridesCorePositionableStream: known
	self
		addTo: known
		forBehavior: PositionableStream
		overrides: {
			#'setToEnd' -> 'setToEnd

"Sets the receiver''s position to the end of the sequence of objects."

self positionL: itsCollection size + 1
'.
	}