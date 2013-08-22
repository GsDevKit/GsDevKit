navigation
restrictToNavigation: navigation 
	children := (self children select: [ :e | e navigation = navigation ])