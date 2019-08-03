library("testthat")
library("tidyverse")
library("magrittr")
source("R/diagnosis_util.R")
source("R/consult_util.R")
source("R/patient_util.R")

VERBOSE = FALSE

# override with test data
Util.CommunityPaths <- function() {
  list.dirs("data/test")[-1] # element 1 is "data/test" itself
}

PreppedTestConsults <- function() {
  rawConsults <- Con.GetConsults()
  consults <- Con.PrepareGeneralConsultData(rawConsults, patients)
  patients <- Pt.GetCleanedTable()
  return(consults)
}

DX_SET_MEMBERS <- "PIH:DIAGNOSIS;PIH:CLINICAL IMPRESSION DIAGNOSIS CONFIRMED;PIH:Diagnosis order"

test_that("DiagnosisMapper returns the expected results", {
  output <- Dx._DiagnosisMapper(2)
  expect_equal(output$obsValue, NA)
  expect_equal(output$setMembers, DX_SET_MEMBERS)
  expect_equal(output$setValues, "PIH:151;PIH:PRESUMED;PIH:primary")
})

test_that("PrepareDiagnosisData produces the expected output", {
  rawDx <- Dx.GetDiagnoses()
  consults <- PreppedTestConsults()
  output <- Dx.PrepareDiagnosisData(rawDx, consults)
  expect_equal("PIH:Visit Diagnoses", output$concept[1])
  expect_equal(DX_SET_MEMBERS, as.character(output$setMembers[1]))
  expect_equal("PIH:151;PIH:PRESUMED;PIH:primary", output$setValues[1])
})
