operations
deleteDirectory: localDirName
  "Delete the directory with the given name in this directory. Fail if the path is bad or if a directory by that name does not exist."

  (GsFile
    _removeDirectory: (self fullNameFor: localDirName)
    onClient: self onClient) == nil
    ifTrue: [ 
      self
        error:
          'Delete directory: ' , localDirName printString , ' failed: '
            , (GsFile lastErrorString: self onClient) ]