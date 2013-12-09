operations
createDirectory: localDirName
  "Create a directory with the given name in this directory. Fail if the name is bad or if a file or directory with that name already exists."

  (GsFile
    _createDirectory: (self fullNameFor: localDirName)
    onClient: self onClient
    mode: nil) == nil
    ifTrue: [ 
      ^ self
        error:
          'Could not create directory: ' , (GsFile lastErrorString: self onClient) ]