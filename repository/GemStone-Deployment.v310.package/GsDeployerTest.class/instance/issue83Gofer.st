tests Issue 83
issue83Gofer
  | repo g |
  g := (Smalltalk at: #'Gofer') new.
  g disablePackageCache.
  repo := self monticelloRepository.
  g repository: repo.
  ^ g