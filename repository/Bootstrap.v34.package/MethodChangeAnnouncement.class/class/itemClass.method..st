instance creation
itemClass: aClass method: aGsMethod

	^self new 
		item: aGsMethod;
		itemClass: aClass; 
		itemSelector: aGsMethod selector;
		itemProtocol: (aClass categoryOfSelector: aGsMethod selector);
		yourself