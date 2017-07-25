*monticello
comment

" As of GS/64 3.1, comments are now recorded in the class extraDict
  dictionary under the key #comment.  Comment information formerly
  recorded as a GsClassDocumentation under the key #description are
  converted to a string and placed under #comment during DB 
  conversion/upgrade. "

  |  cmt |
  (self _extraDictAt: #description) 
    ifNil: [ cmt := self _extraDictAt: #comment]
    ifNotNil: [:desc | "Bug #42589"
	cmt := desc detailsAboutClass].
  ^ cmt isNil ifTrue: [ '' ] ifFalse: [ cmt ]
