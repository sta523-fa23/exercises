test_that("Fizzbuzz", {
  expect_equal(fizzbuzz(1), "1")
  expect_equal(fizzbuzz(3), "fizz")
  expect_equal(fizzbuzz(5), "fizz")

  expect_error(fizzbuzz(-1))
})
