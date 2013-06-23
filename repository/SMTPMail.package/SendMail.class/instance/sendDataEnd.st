private
sendDataEnd

	^self sendCrlfString: (String with: Character cr with: Character lf with: $. ).
