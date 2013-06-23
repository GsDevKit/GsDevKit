testing
isLegalUTF8: aUtf8

	| len char ch a index |
	ch := aUtf8 at: 1.
	len := (self trailingBytesForUTF8: ch) + 1.
	len > 4 ifTrue: [ ^false].
	len == 4
		ifTrue: [
			a := aUtf8 at: 4.
			(a < 16r80 or: [a > 16rBF]) ifTrue: [ ^false ].
			len := 3].
	len == 3
		ifTrue: [
			a := aUtf8 at: 3.
			(a < 16r80 or: [a > 16rBF]) ifTrue: [ ^false ].
			len := 2].
	len == 2
		ifTrue: [
			a := aUtf8 at: 2.
			(a  > 16rBF) ifTrue: [ ^false ].
			ch  == 16rE0 
				ifTrue: [a < 16rA0 ifTrue: [ ^false ]]
				ifFalse: [
					ch  == 16rED
						ifTrue: [a > 16r9F ifTrue: [ ^false ]]
						ifFalse: [
							ch  == 16rF0
								ifTrue: [a < 16r90 ifTrue: [ ^false ]]
								ifFalse: [
									ch  == 16rF4 
										ifTrue: [a > 16r8F ifTrue: [ ^false ]]
										ifFalse: [a < 16r80 ifTrue: [ ^false ]]]]].
			len := 1].
	len == 1
		ifTrue: [
			(ch >= 16r80 and: [ ch <= 16rC2 ]) ifTrue: [ ^false ]].
	(ch > 16rF4) ifTrue: [ ^false ].
	^true
