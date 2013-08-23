private
addToPureExportSet: anObject

	System perform: #'_add:toGciSet:' with: anObject with: 39  "PureExportSet"