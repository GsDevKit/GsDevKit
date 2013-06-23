private
sendPlainMessageData

	^(self write: self plainMessage) == self plainMessage size
