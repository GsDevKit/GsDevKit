*change-notification
removeFromClassHistory
  "Remove class from the receiver's class history"

  self classHistory removeVersion: self.
  MethodVersionHistory uniqueInstance removeVersionsForClass: self.
  MCMethodDefinition resetCachedDefinitions