private
userHasSelectedClass: nonMetaclassName
	classes remove: nonMetaclassName ifAbsent: [].
	classes addFirst: nonMetaclassName.
	classes size > self maximumSize
			ifTrue: [classes removeLast]