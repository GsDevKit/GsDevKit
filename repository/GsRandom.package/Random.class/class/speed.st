Testing
speed
       "Random speed will benchmark the efficiency of random number
generation"

|rand|

rand := self new.

^(System millisecondsToRun: [ 1000 timesRepeat: [rand next]
  ]) / 1000.0