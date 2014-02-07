*bootstrap-instance creation
new
  "Creates and returns a new instance of ClassOrganizer with a root of Object,
 and using the symbolList of the current UserProfile."

  ^ self _newWithRoot: Object symbolList: GsSession currentSession symbolList