accessing
setLastModFileDateTimeFrom: aSmalltalkTime
	| unixTime |
	unixTime := aSmalltalkTime -  2177424000.		"PST?"
	lastModFileDateTime := self unixToDosTime: unixTime