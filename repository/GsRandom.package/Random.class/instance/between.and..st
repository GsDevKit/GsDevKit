accessing
between: min and: max

   "Return a random value x such that min <= x < max"

  ^(self next * (max - min)) + min