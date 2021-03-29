instance creation
interaction: aString object: anObject
  ^ (self new label: aString object: anObject)
    priority: self interaction;
    yourself