accessing columns
pushColumn: aColumn
	self columns addLast: aColumn. 
	self changed: #columns.