as yet unclassified
classes
	^ (SystemNavigation default allClassesImplementing: query)
		collect: [:ea | OBClassNode on: ea]