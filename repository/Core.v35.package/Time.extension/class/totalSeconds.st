*core-squeak
totalSeconds

"Returns an Integer that represents the receiver in units of seconds since
 midnight January 1, 1901, UTC (not local, because local time is too expensive)."

^(DateAndTime secondsUTC: DateAndTime secondsSince2001 offset: Duration zero) asSeconds asInteger  + 3155760000