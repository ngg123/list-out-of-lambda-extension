#
# This is a file meant to demonstrate the LooL cardinal
# number system, implemented in R without the 'if' statement
# or intrinsic boolean variables
#

#
# Test function
#
# Note the number larger than a few hundred will cause R to run out of stack space!
#
testFunc <- function(){
  print(paste0("floor(((3*4+1)^2)/5)= 33: ",print.num(div(pow(add(mul(three,four),one),two),five))))
  print(paste0("(2^((2*5)-2) = 256: ",print.num(pow(two,sub(mul(two,five),two)))))
}


########################
## Bool things
########################

iff <- function(bool,ti,fe){
  bool(ti,fe)
}

#
# Bool selectors
#

true <- function(t,f){t}
false <- function(t,f){f}

#
# Bool operators
#
not <- function(bool){bool(false,true)}

and <- function(boola,boolb){
  boola(boolb,false)
}

or <- function(boola,boolb){
  boola(true,boolb)
}

xor <- function(boola,boolb){
  boola(not(boolb),boolb)
}


##########################
## List things
##########################
empty.list <- function(sel){sel(NA,NA,true)}

prepend <- function(hd,tl){
  function(sel){sel(hd,tl,false)}
}

#
# List selectors
#
head <- function(lis){
  lis(function(hd,tl,eol){hd})
}

tail <- function(lis){
  lis(function(hd,tl,eol){tl})
}

is.empty <- function(lis){
  lis(function(hd,tl,eol){eol})
}


#
# Number utility functions
#
zero <- empty.list

print.num <- function(num,i=0){iff(eq.zero(num),i,print.num(dec(num),i+1))}

eq.zero <- function(num){is.empty(num)}

#
# Number operators
#

inc <- function(num){prepend(empty.list,num)}
one <- inc(zero)

dec <- function(num){tail(num)}

add <- function(numa,numb){
  iff(is.empty(numb),
      numa,
      # else 
      add(inc(numa),dec(numb))
  )
}

sub <- function(numa,numb){
  iff(eq.zero(numa),zero,
      iff(eq.zero(numb),numa,
          sub(dec(numa),dec(numb))))
}

mul <- function(numa, numb){
  iff(eq.zero(numb),zero,add(numa,mul(numa,dec(numb))))
}

pow <- function(numa,numb){
  iff(eq.zero(numb),one,mul(numa,pow(numa,dec(numb))))
}

gt <- function(numa,numb){
  iff(eq.zero(numa),false,
      iff(eq.zero(numb),true,
          gt(dec(numa),dec(numb))))
}

lt <- function(numa,numb){
  gt(numb,numa)
}

equal <- function(numa,numb){
  iff(xor(eq.zero(numa),eq.zero(numb)),false,
      iff(and(eq.zero(numa),eq.zero(numb)),true,
          equal(dec(numa),dec(numb))))
}

div <- function(numa,numb){
  iff(or(eq.zero(numa),lt(numa,numb)),zero,
      iff(eq.zero(numb),empty.list,
          inc(div(sub(numa,numb),numb))))
}

one <- inc(zero)
two <- inc(one)
three <- add(two,one)
four <- mul(two,two)
five <- inc(four)

