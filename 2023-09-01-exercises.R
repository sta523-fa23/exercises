# Exercise 1

## Part 1

c(1, NA+1L, "C") # double, int, char -> char

c(1L / 0, NA)    # dbl, log -> dbl

c(1:3, 5)        # Int, Int, Int, dbl -> dbl

c(3L, NaN+1L)    # Int, dbl -> dbl 

c(NA, TRUE)      # lgl, lgl -> lgl


## Part 2

### character > double > integer > logical


# Exercise 2

f = function(x) {
  # Check small prime
  if (x > 10 || x < -10) {
    stop("Input too big")
  } else if (x %in% c(2, 3, 5, 7)) {
    cat("Input is prime!\n")
  } else if (x %% 2 == 0) {
    cat("Input is even!\n")
  } else if (x %% 2 == 1) {
    cat("Input is odd!\n")
  }
}



f(1)
f(3)
f(8)
f(-1)
f(-3)
f(1:2)
f("0")
f("3")
f("zero")
