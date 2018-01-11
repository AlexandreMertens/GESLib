library(testthat)
library(GESLib)

context("read from journal tools test")


test_that("Date are not accepted when '2000' is missing in year", {
  expect_error(checkDate(as.POSIXct("17/10/21 18:47:22")),"Date.*")
})
