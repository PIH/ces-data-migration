library("testthat")
library("tidyverse")

ptsData <- read_csv("data/output/patients.csv")
regEncsData <- read_csv("data/output/encounters-registration.csv")
# regObsData <- read_csv("data/output/obs-registration.csv")
vitEncsData <- read_csv("data/output/encounters-vitals.csv")
# vitObsData <- read_csv("data/output/obs-vitals.csv")
conEncsData <- read_csv("data/output/encounters-consults.csv")

test_that("UUIDs all match up", {
  expect_true(all(regEncsData$`Patient UUID` %in% ptsData$uuid))
  expect_true(all(vitEncsData$`Patient UUID` %in% ptsData$uuid))
  expect_true(all(conEncsData$`Patient UUID` %in% ptsData$uuid))
})
