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

test_that("coded12 returns as expected", {
  expect_equal(length(coded12ReactiveText(1)), 3)
  expect_equal(coded12ReactiveText(1),
               c(obsValue = "Reactivo", setMembers = NA, setValues = NA))
  expect_equal(coded12ReactiveText(2),
               c(obsValue = "No reactivo", setMembers = NA, setValues = NA))
  expect_true(is.na(coded12ReactiveText(NA)))
})

test_that("GetConsults gets some items", {
  output <- Con.GetConsults()
  expect_equal(nrow(output), 4)
})

test_that("GetConsults appends community name", {
  output <- Con.GetConsults()
  expect_equal(output$commName, c("Salvador", "Salvador", "Salvador", "Soledad"))
})

test_that("PrepareConsultObs produces the expected output", {
  rawConsultObs <- tribble(
    ~"accessCol", ~"accessVal",
    "Peso", "30,2",
    "Talla.con", "160"
  )
  output <- Con._PrepareConsultObs(rawConsultObs, VITALS_CONSULT_MAPPING_SPEC)
  expect_equal(c("PIH:WEIGHT (KG)", "PIH:HEIGHT (CM)"), output$concept)
  expect_equal(c("30.2", "160"), output$obsValue)
})

test_that("PrepareVitalsData returns data with the right number of rows, no NA dates, no NA values", {
  consults <- PreppedTestConsults()
  output <- Con.PrepareVitalsData(consults)
  expect_equal(sort(names(output)), c("encounters", "obs"))
  expect_true(all(!is.na(output$encounters$Date)))
  expect_true(all(!is.na(output$obs$Value)))
})

test_that("Vitals UUIDs are unique and all match up with an entry from patients", {
  patients <- Pt.GetCleanedTable()
  consults <- PreppedTestConsults()
  output <- Con.PrepareVitalsData(consults)
  # unique
  expect_true(all(!duplicated(output$encounters$uuid)))
  expect_true(all(!duplicated(output$obs$uuid)))
  # present in patients
  expect_true(all(output$encounters$`Patient UUID` %in% patients$ptUuid))
})

test_that("PrepareConsultFormData returns data with the right number of rows,
          no NA dates, no NA values/setValues, non-list Value column", {
  consults <- PreppedTestConsults()
  output <- Con.PrepareConsultFormData(consults)
  expect_equal(sort(names(output)), c("encounters", "obs"))
  expect_true(all(!is.na(output$encounters$Date)))
  expect_true(
    all(
      !is.na(output$obs$Value) | (
        !is.na(output$obs$`Set Members`) &
        !is.na(output$obs$`Set Member Values`)
      )
    ),
    # as_tibble here doesn't do anything to help formatting, but one day maybe
    # test_that will be better and learn to respect tibbles
    as_tibble(output$obs[c("Value", "Set Members", "Set Member Values")])
  )
  expect_true(class(output$obs$Value) != "list")
})

test_that("ProduceUncodedColString works as expected", {
  ncharMapper <- function(x) c(obsValue = nchar(x), setMembers = NA, setValues = NA)
  idMapper <- function(x) c(obsValue = x, setMembers = NA, setValues = NA)
  appendSpec <- tribble(
    ~"accessCol", ~"friendlyName", ~"valueMapper",
    "name", "Name Length", ncharMapper,
    "mom",  "Mom's Name", idMapper
  )
  input <- tribble(
    ~"name", ~"mom", ~"other",
    "foo",   "Dorothy", 0,
    "barrr", NA,  1
  )
  output <- Con._ProduceUncodedColString(input, appendSpec)
  expect_true(class(output) == "character")
  expect_equal(length(output), 2)
  expect_equal(output[1], "Name Length: 3\nMom's Name: Dorothy")
  expect_equal(output[2], "Name Length: 5")
})


test_that("PrepareConsultFormData appends extra columns to end of consult note", {
  consults <- PreppedTestConsults()
  output <- Con.PrepareConsultFormData(consults)
  consWithFramingham <- output$obs[
    !is.na(output$obs$Value) &
    startsWith(output$obs$Value, "A consult row"),
  ]
  expect_true(endsWith(consWithFramingham$Value, "Framingham: 5"),
              paste("Actual note:", consWithFramingham$Value))
})

test_that("Consult UUIDs are unique and all match up with an entry from patients", {
  patients <- Pt.GetCleanedTable()
  consults <- PreppedTestConsults()
  output <- Con.PrepareConsultFormData(consults)
  # unique
  expect_true(all(!duplicated(output$encounters$uuid)))
  expect_true(all(!duplicated(output$obs$uuid)))
  # present in patients
  expect_true(all(output$encounters$`Patient UUID` %in% patients$ptUuid))
})

