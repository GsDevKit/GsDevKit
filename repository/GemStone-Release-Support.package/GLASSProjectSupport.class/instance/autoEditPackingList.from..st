driver
autoEditPackingList: packingListPath from: filenames


	| file packingStream beginMarker endMarker |
	packingListPath == nil ifTrue: [ ^self ].
	beginMarker := '#---------------BEGINMARKER for Automatically generated mcz packinglist'.
	endMarker := '#---------------ENDMARKER for Automatically generated mcz packinglist'.
	file := GsFile openReadOnServer: packingListPath.
	[ packingStream := ReadStream on: file contents ] ensure: [ file close ].
	file := GsFile openWriteOnServer: packingListPath.
	[[ packingStream atEnd ] whileFalse: [ | line |
		line := packingStream nextLine.
		line = beginMarker 
			ifTrue: [
				line := packingStream nextLine.
				[ line ~= endMarker ] whileTrue: [ 
					packingStream atEnd ifTrue: [ ^self error: 'Never found the ENDMARKER in the packing file' ].
					line := packingStream nextLine ].
				file nextPutAll: beginMarker; cr.
				filenames asArray sort do: [:filename |
					self extensionAndAttributes first first = '30'
						ifTrue: [ file nextPutAll: '  copyssmcz ',  filename, '.mcz' ]
						ifFalse: [
							file 
								nextPutAll: '  copy seaside/monticello/repository/', 
									filename, 
									'.mcz $ARCHBASE/examples/monticello/repository/', 
									filename, 
									'.mcz    bin' ].
					file cr ].
				file nextPutAll: endMarker; cr ]
			ifFalse: [ file nextPutAll: line; cr ]]] ensure: [ file close ]