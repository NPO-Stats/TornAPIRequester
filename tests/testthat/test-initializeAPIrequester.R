test_that("initialization from environment variable works", {
  # This test assumes that we have an environment variable containing the data we need to initialize from:
  skip_if_not("TAPIR_INIT" %in% names(Sys.getenv()))

  # and since it is in the environment, initializeAPIrequester requires no arguments:
  expect_true(initializeAPIrequester())
})
