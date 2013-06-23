private
fullName: fileName type: type
	| class |
	(type == #binary or: [ type == #serverBinary ])
		ifTrue: [ class := ServerFileDirectory ].
	(type == #text or: [ type == #serverText ] )
		ifTrue: [ class := ServerFileDirectory ].
	type == #clientBinary
		ifTrue: [ class := FileDirectory ].
	type == #clientText
		ifTrue: [ class := FileDirectory ].
	^ class default fullNameFor: fileName