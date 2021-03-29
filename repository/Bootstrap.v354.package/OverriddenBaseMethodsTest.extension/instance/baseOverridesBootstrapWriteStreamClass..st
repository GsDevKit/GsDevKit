*bootstrap
baseOverridesBootstrapWriteStreamClass: known
	self
		addTo: known
		forBehavior: WriteStream class
		overrides: {
			#'with:' -> 'with: aCollection

"Returns an instance of the receiver open for writing. that is positioned at the end of 
 aCollection."
 
   ^(self on: aCollection) setToEnd; 
	yourself
'.
	}