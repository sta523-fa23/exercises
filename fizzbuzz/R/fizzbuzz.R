
test = function() {
  print("Hello world!")
}

#' @title The fizzbuzz function
#'
#' @description
#' Implements the fizzbuzz function for **numeric** variables
#'
#' @param x A numeric vector of *positive* finite values
#'
#' @return A character vector
#'
#' @examples
#' fizzbuzz(1:5)
#' fizzbuzz(5:1)
#' fizzbuzz(15)
#'
#'
#' @export
fizzbuzz = function(x) {
  stopifnot(is.numeric(x))
  stopifnot(all(x>0))
  stopifnot(all(is.finite(x)))

  dplyr::case_when(
    x %% 3 == 0 & x %% 5 == 0 ~ "fizzbuzz",
    x %% 3 == 0 ~ "fizz",
    x %% 5 == 0 ~ "buzz",
    TRUE ~ as.character(x)
  )
}
