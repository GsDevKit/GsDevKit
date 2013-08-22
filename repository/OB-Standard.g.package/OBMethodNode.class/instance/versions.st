navigating
versions
	^ (OBGemstoneMethodVersion versionsOfMethod: selector in: self theClass)
		collect: [:ea | OBMethodVersionNode on: ea]