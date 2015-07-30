rm(list=ls())
#
# I'm going to abbreviate List out of Lambda, written by fellow RIT Alum
# Steve Losh, as LooL.  
#
# This R project is an extension of the original LooL, inspired by 
# Steve's challenge to reimplement it without using Javascript's 
# intrinsic boolean type.  Besides re-implementing LooL in R, I have
# written it without using R's intrinsic boolean variable (and without
# using R's intrinsic comparison operators or 'if' constructs).
# 
# It would be most useful to read (and understand) LooL 
# (http://stevelosh.com/blog/2013/03/list-out-of-lambda/) before 
# diving into this code.  
#
#
# Let's start about halfway through LooL (right before the heading
# "A Brief Intermission").
#
# To review, a LooL list is a function which takes a function as its
# argument and returns the result of that function invoked on three
# arguments: The head of the list, tail of the list, and boolean flag
# which indicates whether the current node is the end of the list.
#
# The empty list passes the value NA (though any placeholder could do)
# for the head and tail arguments, and the value representing 'truthiness'
# for the end-of-list argument.  Steve Losh's implementation would look
# like this:
empty.list <- function(sel){sel(NA,NA,TRUE)}
#
# The prepend function attaches a new element to the front of the list,
# so it takes two parameters (head and tail) and returns a list (a 
# function with three parameters as above).  In the case of list returned
# by prepend, the end-of-list parameter should be 'falsy.'
#
prepend <- function(hd,tl){
  function(sel){sel(hd,tl,FALSE)}
}
#
# Extracting the head or tail of a list is straightforward: An a appropriate
# 'selector' is passed to the list, which is a function of three arguments 
# (head of list, tail of list, and value indicating endness of list)
# 
# 
head <- function(lis){
  lis(function(hd,tl,eol){hd})
}

tail <- function(lis){
  lis(function(hd,tl,eol){tl})
}
#
# Checking whether the list is empty is also simple: The selector returns
# the end-of-list argument:
#
is.empty <- function(lis){
  lis(function(hd,tl,eol){eol})
}

#
# Steve Losh uses lists to represent cardinal numbers (with the amount of 
# list elements ahead of the empty list being equivalent to the cardinality).
# I'll put the function definitions here, but they are equivalent to the 
# objects with the same names in LooL.
#
# Notice that the print.num function takes advantage of R's 'default parameter'
# featur, so it can be called as: print.num(three) instead of print.num(three,0)
#
zero <- empty.list
eq.zero <- function(num){is.empty(num)}
inc <- function(num){prepend(empty.list,num)}
dec <- function(num){tail(num)}
print.num <- function(num,i=0){if(eq.zero(num)){i}else{print.num(dec(num),i+1)}}

#
# Functions which treat lists as numbers (e.g., add, sub, mul, div)
# commonly need to perform branching.  Take a look at add:
#
add <- function(numa,numb){
  if(is.empty(numb)){
    numa
  } else{
    add(inc(numa),dec(numb))
  }
}
#
# Check that this works:
#
# > print.num(add(inc(zero),inc(inc(zero))))
# [1] 3
#
# is.empty() returns a boolean, which is used by R's 'if' statement to 
# conditionally branch to another part of the code.  But what if we didn't
# have booleans?  Or if statements?
#
#
# Let's start building Boolean out of Lambda (BooL) by building a conditional
# branching block that doesn't use the intrinsic if statment built into R:
#
# iff is a function which should closely follow the symmantics of
# the intrinsic if function: if the first argument is truthy, then return 
# the second argument, otherwise return the third.
#
# Like the LooL list, the first argument to iff is a selector, to which
# we will pass other parameters.  Differently, the values passed to the 
# selector are the second and third arguments to iff.
#
iff <- function(bool,ti,fe){
  bool(ti,fe)
}
#
# We can define basic true and false selector constants functions
# which always return either true or false.
#
true <- function(t,f){t}
false <- function(t,f){f}
#
# Almost all of the machinery defined above for LooL lists is compatible
# with this new regieme.  All we really need to change is the truthy or 
# falsy value passes to a selector in the list definition, and any
# conditional branching statements:
# 
# In empty.list, we change TRUE (the intrinsic boolean) to true (the 
# selector function we defined above).
#
empty.list <- function(sel){sel(NA,NA,true)}
zero <- empty.list
# 
# prepend only requires changing FALSE to false
#
prepend <- function(hd,tl){
  function(sel){sel(hd,tl,false)}
}
#
# In print.num, we change the if construct from this:
# print.num <- function(num,i=0){if(eq.zero(num)){i}else{print.num(dec(num),i+1)}}
# to this:
print.num <- function(num,i=0){iff(eq.zero(num),i,print.num(dec(num),i+1))}
#
# The add function is adjusted similarly:
#
add <- function(numa,numb){
  iff(is.empty(numb),
      numa,
  # else 
      add(inc(numa),dec(numb))
  )
}
# Check that it still works:
#
# > print.num(add(inc(zero),inc(inc(zero))))
# [1] 3
#
# Some of the other LooL functions for working with lists
# require the use of binary operators like and,or,xor,not:
#
# Let's start with the simplest. The boolean selector will
# return the first argument when it is a truthy selector and
# the second when it is a falsy one.  So we pass the falsy
# boolean selector constant to bool as the first argument
# and the truthy constant as the second.
#
not <- function(bool){bool(false,true)}
#
# The rest of the boolean operators are defined similarly:
#
and <- function(boola,boolb){
  boola(boolb,false)
}

or <- function(boola,boolb){
  boola(true,boolb)
}

xor <- function(boola,boolb){
  boola(not(boolb),boolb)
}


