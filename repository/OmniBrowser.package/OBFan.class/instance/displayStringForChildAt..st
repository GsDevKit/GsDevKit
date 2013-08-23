selecting
displayStringForChildAt: index
	^ self displayStringForChild: (self children at: index ifAbsent: [^''])