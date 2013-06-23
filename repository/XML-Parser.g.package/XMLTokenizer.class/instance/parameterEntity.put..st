entities
parameterEntity: refName put: aReference
	"Only the first declaration of an entity is valid so if there is already one don't register the new value."
	self parameterEntities at: refName ifAbsentPut: [aReference]