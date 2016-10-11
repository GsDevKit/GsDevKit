*bootstrap
USDateFormat

"Returns a String that expresses the date of the receiver. The date is in 
 United States format, month first (MM/DD/YYYY)."

^self asStringUsingFormat: #(2 1 3 $/ 1 1 $: false )