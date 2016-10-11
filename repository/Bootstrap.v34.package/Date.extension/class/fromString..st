*bootstrap-instance creation
fromString: aString

"Read a Date from aString in any of the forms:  
	
		<day> <monthName> <year>		(5 April 1982; 5-APR-82)  
	
		<monthName> <day> <year>		(April 5, 1982)  
	
		<monthNumber> <day> <year>		(4/5/82) 
			<day><monthName><year>			(5APR82)"

	^self readFrom: (ReadStream on: aString).