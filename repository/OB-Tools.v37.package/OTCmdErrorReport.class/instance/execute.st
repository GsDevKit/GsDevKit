execution
execute
	| str |
	str := WriteStream on: String new.
	target errorReportOn: str.
	str contents inspect