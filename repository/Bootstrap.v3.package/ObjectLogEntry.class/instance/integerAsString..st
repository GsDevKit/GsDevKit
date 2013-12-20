private
integerAsString: anInteger
| aString |

(anInteger < 100)
  ifTrue: [
	(anInteger < 10)
  		ifTrue: [aString := '00' , anInteger asString]
		ifFalse: [aString := '0' , anInteger asString]]
  ifFalse: [aString := anInteger asString].
^ aString