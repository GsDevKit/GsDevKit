instance creation
interaction: aString
  ^ (self new label: aString object: nil)
    priority: self interaction;
    yourself