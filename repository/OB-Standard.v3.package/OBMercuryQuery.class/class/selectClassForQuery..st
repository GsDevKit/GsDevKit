private
selectClassForQuery: rawQuery 
	^ rawQuery first = $# 
		ifTrue: 
			[(rawQuery allButFirst first isUppercase) 
				ifTrue: [OBMClassRefsQuery]
				ifFalse: [OBMMethodSendersQuery]]
		ifFalse: 
			[(self isClassQuery: rawQuery) 
				ifTrue: [OBMClassQuery]
				ifFalse: [OBMMethodQuery]]