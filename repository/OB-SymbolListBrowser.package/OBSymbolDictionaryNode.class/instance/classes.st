navigating
classes
	^ self rawClasses collect: [:cl | 
			(cl asNode)
				symbolDictionary: self symbolDictionary;
				yourself]