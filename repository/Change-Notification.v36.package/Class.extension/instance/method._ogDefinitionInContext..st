*change-notification
method: Class
_ogDefinitionInContext: aUserProfile

"Returns a description of the receiver using object names taken from the given
 UserProfile. Crafted for OG-Standard browsers"
  | superC |
^ self
    _ogDefinitionInContext: aUserProfile
    className: name
    superClassName: ((superC := superClass) ifNil:[ 'nil' ] ifNotNil:[ superC name ])
