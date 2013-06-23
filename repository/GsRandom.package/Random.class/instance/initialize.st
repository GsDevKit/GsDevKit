Private
initialize

  "Initialize an instance, seeded by the time of day"

  self seed: (System _timeMs \\ 2147483646) + 1