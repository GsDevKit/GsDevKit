accessing
centralDirectoryHeaderSize

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName.
	systemFileComment := fileComment.
	systemCdExtraField := cdExtraField.
	^ 46 + systemFileName size + systemCdExtraField size + systemFileComment size