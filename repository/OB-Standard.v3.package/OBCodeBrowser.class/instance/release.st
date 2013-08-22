morphic
release
    "work-around for bug #7119"
    super release.
    self panels do: [:panel | panel release].