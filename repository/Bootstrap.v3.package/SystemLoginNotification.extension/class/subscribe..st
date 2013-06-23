*bootstrap-public
subscribe: anObjectName
	"subscribe using the name of the object, 'System myUserProfile objectNamed:' used to look up the object in symbol dictionaries"
	
	self subscriptions add: anObjectName
