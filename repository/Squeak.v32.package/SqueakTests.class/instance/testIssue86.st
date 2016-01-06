testing
testIssue86
  "https://github.com/GsDevKit/GsDevKit/issues/86"

  self assert: (Year year: 2016) previous = (Year year: 2015).
  self assert: (Year year: 2015) previous = (Year year: 2014).
  self assert: (Year year: 2014) previous = (Year year: 2013).
  self assert: (Year year: 2012) previous = (Year year: 2011).
  self assert: (Year year: 2011) previous = (Year year: 2010)