special cases
nonImplementedSelectorsGLASS_35
	"The are slectors tht are not implemented in base image (STON and GemStone-Co,mpression) that are only an issue in GLASS ... already addressed in GLASS1"

	^#(_decompressZipArchive ),														"GemStone-Compression"
		#(parseListSingleton parseMapDo: writeObject:listSingleton: writeObject:do:
			encodeList: writeMap: encodeMap: mapClass writeBoolean: parseListDo: ),	"STON"
		#( destroy ) 																	"HTTPSocket"