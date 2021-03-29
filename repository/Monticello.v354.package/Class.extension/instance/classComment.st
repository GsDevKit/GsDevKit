*monticello
classComment
	"bypass default comment for 3.5.x and beyond"

  (self _extraDictAt: #'comment') ifNotNil: [ :cmt | ^ cmt ].
	^''
