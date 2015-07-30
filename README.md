# list-out-of-lambda-extension
A solution to a challenge posed by Steve Losh in List out of Lambda

Steve Losh's List Out Of Lambda (http://stevelosh.com/blog/2013/03/list-out-of-lambda/) makes a statement and poses a challenge just before the section entitled "A Brief Intermission."  He notes that List Out Of Lambda uses only two language features: functions and true/false boolean values.  I count three, if you include the conditional branching provided by the 'if' statement.  

This R project reimplements List Out Of Lambda in R, and answers Steve's challenge: R's built-in boolean type is not used (even implicitly), nor is R's if statement.  The standard boolean operators are implemented for use on lambda-booleans.  I have also implemented the cardinal number system (and operators) to demonstrate that it all works (though R's default stack size limits the system to numbers less than about 300).
