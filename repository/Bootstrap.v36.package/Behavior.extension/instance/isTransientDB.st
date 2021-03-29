*bootstrap
isTransientDB

"Returns true if instances of the receiver are transientDB objects.  Otherwise,
 returns false."

^ ((format bitAnd: 16r1000) == 16r1000)