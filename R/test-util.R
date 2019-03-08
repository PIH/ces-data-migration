library("testthat")
source("R/util.R")

test_that("AnyEntryNotNa selects the correct rows", {
  input <- tibble(
    foo = c(1, 2, NA),
    bar = c(3, NA, NA),
    baz = c(5, NA, 6)
  )
  
  output <- Util.AnyEntryNotNa(input, c("foo", "bar"))
  expect_equal(output, c(TRUE, TRUE, FALSE))
})

test_that("AppendClinicNames produces the expected data", {
  tblPerSite <- list(
    tibble(foo = c(0, 1), bar = c(2, 3), baz = c(4, 5)),
    tibble(foo = c(3, 2), bar = c(4, 8), baz = c(2, 6))
  )
  sites <- c("data/test/Earth/Thing.csv", "data/test/Space/Thing.csv")
  output <- Util.AppendClinicNames(tblPerSite, sites)
  expect_equal(output[[1]]$commName, c("Earth", "Earth"))
  expect_equal(output[[2]]$commName, c("Space", "Space"))
})

test_that("CsvToTibble generates a tibble with the right dimensions", {
  output <- Util.CsvToTibble("data/test/Salvador/Pacientes.csv")
  expect_equal(ncol(output), 45)
  expect_equal(nrow(output), 10)
})

test_that("GetDupeGroups produces a list with grouped rows", {
  testData <- tribble(
    ~"name", ~"age", ~"keyCol",
    "foo",   21,     "fooKey",
    "bar",   20,     "barKey",
    "baz",   25,     "fooKey",
    "bar",   22,     "barKey",
    "qux",   23,     "quxKey"
  )
  output <- Util.GetDupeGroups(testData, "keyCol")
  expect_equal(length(output), 2)
  expect_equal(sort(names(output)), c("barKey", "fooKey"))
  expect_equal(nrow(output[[1]]), 2)
  expect_equal(nrow(output[[2]]), 2)
})

test_that("TransformDate parses dates, but messes up time zones", {
  accessDates <- c("Fri Apr 17 00:00:00 CDT 2015",
                   "Mon Oct 20 00:00:00 ABC 2014")
  
  output <- Util.TransformDate(accessDates)
  expect_equal(output[[1]], "2015-04-16T19:00:00-0500")
  expect_equal(output[[2]], "2014-10-19T19:00:00-0500")
})

test_that("TransformDate defaults bad dates to 1990-01-01", {
  accessDates <- c("Fri Apr 17 00:00:00 CDT 1900",
                   "Mon Oct 20 00:00:00 PST 2200")
  
  output <- Util.TransformDate(accessDates)
  expect_equal(output[[1]], "1989-12-31T18:00:00-0600")
  expect_equal(output[[2]], "1989-12-31T18:00:00-0600")
})

test_that("TransformDate converts NA to 1990-01-01", {
  accessDates <- c(NA)
  output <- Util.TransformDate(accessDates)
  expect_equal(output[[1]], "1989-12-31T18:00:00-0600")
})

test_that("YesNoConceptForBoolean returns expected values", {
  yes <- Util.YesNoConceptForBoolean(TRUE)
  expect_equal(yes, "PIH:YES")
  no <- Util.YesNoConceptForBoolean(FALSE)
  expect_equal(no, "PIH:NO")
})
