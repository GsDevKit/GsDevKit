actions
removeOldClasses
  self queryOldClasses do: [ :cl | self dictionary removeKey: cl ]