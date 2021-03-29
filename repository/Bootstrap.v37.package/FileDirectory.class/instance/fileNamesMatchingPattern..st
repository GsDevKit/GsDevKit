operations
fileNamesMatchingPattern: patternArray
	"
	FileDirectory default fileNamesMatchingPattern: #(#( $* ))
	FileDirectory default fileNamesMatchingPattern: #( #( $* '.image') #($*'.changes') )
	"
	
	| files |
	files := OrderedCollection new.
	
	patternArray do: [ :pat | 
		files addAll: (self fileNames select: [:name | name matchPattern: pat ]) ].
	
	^files