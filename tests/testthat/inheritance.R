
context("Object Inheritance")

test_that("Different Unit types inherit from Unit_Type Class", {
  M <- measure()
  expect_true(is_Weight(M@Weight))
  expect_true(is_Distance(M@Distance))
  expect_true(is_Time(M@Time))
  expect_true(is_Temperature(M@Temperature))
})

test_that("Missing Unit type in measure results in constant", {
  M <- measure()
  expect_equal(getUnit(M), "constant")
  expect_false(is_Weight(M))
  expect_false(is_Distance(M))
  expect_false(is_Time(M))
  expect_false(is_Temperature(M))
})



