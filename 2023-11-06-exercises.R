## Exercise 1

# Write a set of conditional(s) that satisfies the following requirements,
# 
# If x is greater than 3 and y is less than or equal to 3 then print “Hello world!”
# 
# Otherwise if x is greater than 3 print “!dlrow olleH”
# 
# If x is less than or equal to 3 then print “Something else …”
# 
# stop() execution if x is odd and y is even and report an error, 
# don’t print any of the text strings above.
# 
# Test out your code by trying various values of x and y.


f = function(x, y) {
  if (x %% 2 == 1 & y %% 2 == 0) {
    stop("x is odd and y is even!")
  }
  
  if (x > 3 & y <= 3) {
    print("Hello world!")
  } else if (x > 3) {
    print("!dlrow elloH")
  } else if (x <= 3) {
    print("Something else ...")
  }
}

f(x=1,y=1)
f(x=4,y=1)
f(x=4,y=4)
f(x=5,y=4)



## Exercise 2

z = 1

f = function(x, y, z) {           # x=3, y=1, z=2
  z = x+y                         # z = 3+1 = 4
                                  # 
  g = function(m = x, n = y) {    # m=3, n=1
    m/z + n/z                     # g() = 3/4 + 1/4 = 1
  }                               # 
                                  # 
  z * g()                         # 4 * 1 = 4
}                                 # 

f(1, 2, x = 3)


## Exercise 3

primes = c( 2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

x = c(3,4,12,19,23,51,61,63,78)

res = c()
for ( x in c(3,4,12,19,23,51,61,63,78) ) {
  for (prime in primes) {
    if (x == prime) {
      res = c(res, x)
      break
    }
  }
}

x = c(3,4,12,19,23,51,61,63,78)

x[x %in% primes]

