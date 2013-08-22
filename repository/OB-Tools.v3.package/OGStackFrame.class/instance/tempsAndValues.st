accessing
tempsAndValues

	| stream |
	stream := WriteStream on: String new.
	variables withIndexDo: [ :name :index |
		stream 
			nextPutAll: name;
			nextPutAll: ': '.
		(self tempAt: index) printOn: stream.
		stream cr.
	].
	^stream contents.