instance creation
itemClass: aClass method: aGsMethod oldCategory: cat

	^self new 
		item: aGsMethod;
		itemClass: aClass; 
		itemSelector: aGsMethod selector;
		itemProtocol: (aClass categoryOfSelector: aGsMethod selector);
		oldCategory: cat;
		yourself