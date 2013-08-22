*ob-standard
ogDefinition

"Returns a String containing a GemStone Smalltalk definition for the receiver
 (that is, a subclass creation message crafted for use with the OG-Standard 
  tools).  This method uses the UserProfile of the owner of the current session 
  as the correct context."

"For use with the Topaz run command."

^ self _ogDefinitionInContext: System myUserProfile