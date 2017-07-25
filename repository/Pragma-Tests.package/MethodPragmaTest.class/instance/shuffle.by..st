utilities
shuffle: aCollection by: aRandomGenerator
  | copy |
  copy := aCollection shallowCopy.
  copy size to: 1 by: -1 do: [ :i | 
    | element oneIndex anotherIndex |
    oneIndex := i.
    anotherIndex := (1 to: i) at: (self nextInt: i from: aRandomGenerator).
    element := copy at: oneIndex.
    copy at: oneIndex put: (copy at: anotherIndex).
    copy at: anotherIndex put: element ].
  ^ copy
