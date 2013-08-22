callbacks
accept: aText notifying: aController
	OBUpdateConcentrator default 
		recordUpdatesDuring: [
			^ self 
				withDefinitionDo: [:def | 
					[ def accept: aText notifying: aController ]
						on: OBAnnouncerRequest
						do: [:notification | notification resume: self announcer ]]
				ifNil: [ true ]]