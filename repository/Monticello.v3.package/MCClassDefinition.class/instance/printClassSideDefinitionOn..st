printing
printClassSideDefinitionOn: stream
	
	stream
		nextPutAll: self className;
		nextPutAll: ' class ';
		cr; tab;
		nextPutAll: 'instanceVariableNames: ';
		store: self classInstanceVariablesString