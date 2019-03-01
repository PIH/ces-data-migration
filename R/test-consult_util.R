library("testthat")
library("tidyverse")
library("magrittr")
source("R/consult_util.R")
source("R/patient_util.R")

VERBOSE = FALSE

# override with test data
Util.CommunityPaths <- function() {
  list.dirs("data/test")[-1] # element 1 is "data/test" itself
}

PreppedTestConsults <- function() {
  rawConsults <- Con.GetConsults()
  patients <- Pt.GetCleanedTable()
  consults <- Con.PrepareGeneralConsultData(rawConsults, patients)
  return(consults)
}

test_that("GetConsults gets some items", {
  output <- Con.GetConsults()
  expect_equal(nrow(output), 4)
})

test_that("GetConsults appends community name", {
  output <- Con.GetConsults()
  expect_equal(output$commName, c("Salvador", "Salvador", "Salvador", "Soledad"))
})

test_that("PrepareVitalsData returns acceptable data", {
  consults <- PreppedTestConsults()
  output <- Con.PrepareVitalsData(consults)
  expect_equal(sort(names(output)), c("encounters", "obs"))
  expect_true(all(!is.na(output$encounters$Date)))
})

test_that("Vitals UUIDs all match up with an entry from patients", {
  patients <- Pt.GetCleanedTable()
  consults <- PreppedTestConsults()
  output <- Con.PrepareVitalsData(consults)
  expect_true(all(output$encounters$`Patient UUID` %in% patients$ptUuid))
})