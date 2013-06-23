private
updateCrc
	crcPosition < position ifTrue:[
		bytesWritten := bytesWritten + position - crcPosition + 1.
		crc := self updateCrc: crc from: crcPosition to: position - 1 in: itsCollection.
		crcPosition := position.
	].