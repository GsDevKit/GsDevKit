For Documentation Installation only
installDocumentation

| doc txt |
doc := GsClassDocumentation newForClass: self.

txt := (GsDocText new) details:
'Class Random is a port of Jeff Sutherland''s implementation of the
 Park & Miller random number generator.

From Jeff Sutherland''s original posting:

"In summary...(this is) a generator which has a full period, is
demonstrably random, and can be implemented correctly on almost
any system.  The generator has been exhaustively tested and its
characteristics are well understood... Moreover, it has become a
standard ... subroutine DNUN in the IMSL library and ... DRAND
in the simulation language SLAM II...  we feel confident in
recommending this random number generator as a minimal standard
against which all others should be judged."

This code has received minimal testing on an IBM P75 486 machine
and may break on a Pentium.  It is written in Enfin Smalltalk but
a port to another Smalltalk is trivial.  The validate function works
but any bugs you find are surely mine.  Send me a note at
jsutherland@vmark.com.' .
doc documentClassWith: txt.

self description: doc.