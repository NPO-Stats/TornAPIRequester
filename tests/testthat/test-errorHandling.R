test_that("condition returns a condition", {
  # We create a condition with condition():
  testCon <- condition("test_class_name", "test message")
  # Test that it did indeed get the class we said it should have, and is of
  # class "condition":
  expect_s3_class(testCon, "condition")
  expect_s3_class(testCon, "test_class_name")
  # Test that we got the message we expect:
  expect_identical(testCon$message, "test message")
})
