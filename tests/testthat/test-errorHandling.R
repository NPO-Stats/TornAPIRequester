test_that("condition returns a condition", {
  # We create a condition with condition():
  testCon <- condition("test_class_name", "test message")
  # Test that it did indeed get the class we said it should have, and is of
  # class "condition":
  expect_s3_class(testCon, "condition")
  expect_s3_class(testCon, "test_class_name")
  # Test that we got the message we expect:
  expect_identical(testCon$message, "test message")

  # We expect that if we try to call it with missing class or message, we get
  # an error:
  expect_error(condition("Just one argument!"))
})

test_that("is.condition tests if things are conditions", {
  # We create a condition with condition() and then test that it is
  # what we think it is:
  testCon <- condition("test_condition", "test message")
  expect_true(is.condition(testCon))
  # We also expect that it does not think non-conditions are conditions:
  expect_false(is.condition(23))
  expect_false(is.condition(list(a = "b", c = 200, "unnamed")))
})

test_that("namedStop stops execution with a classed error", {
  # First, we expect that it does indeed give an error:
  expect_error(namedStop("testError","This should be an error!"), class = "testError")

  # We expect that if the user forgets to supply both an error class and a message,
  # we instead get a simpleError:
  expect_error(namedStop("failError"), class = "simpleError")

  # We also expect that errors can be caught by name:
  expect_equal(
    tryCatch(namedStop("catchMeIfYouCan", "I could be hard to catch..."),
             catchMeIfYouCan = function(e) { return("I can!") },
             error = function(e) { return("I can't...")}),
    "I can!"
  )
})
