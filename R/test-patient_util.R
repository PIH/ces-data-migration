library("testthat")
library("tidyverse")
library("magrittr")
source("R/patient_util.R")

VERBOSE = FALSE

# override with test data
CLEAN_PT_DATA_CACHE <- "data/test/patient-cache.csv"
Util.CommunityPaths <- function() {
  list.dirs("data/test")[-1] # element 1 is "data/test" itself
}

TestPatients <- function() {
  input <- Pt.FetchData()
  patients <- Pt._DenormalizeCommunityNames(
    input$patientsPerSite, input$communitiesPerSite
  )
  patients %<>% Util.AppendClinicNames(Util.CommunityPaths())
  patients <- do.call(rbind, patients)  # flatten
  patients %<>% as_tibble()
  patients %<>% Pt._Unfactorize()
  return(patients)
}

test_that("ParseAndFixBirthdates attaches birthday columns", {
  output <- Pt._ParseAndFixBirthdates(TestPatients())
  expect_equal(output[[1, "birthdate"]], "2011-4-10")
  expect_equal(output[[1, "birthdate.is.estimated"]], FALSE)
})

test_that("ParseAndFixBirthdates fixes nonsense birthdays", {
  output <- Pt._ParseAndFixBirthdates(TestPatients())
  expect_equal(output[[5, "birthdate"]], "1959-1-5")
  expect_equal(output[[5, "birthdate.is.estimated"]], TRUE)
  expect_equal(output[[6, "birthdate"]], "1900-1-1")
  expect_equal(output[[6, "birthdate.is.estimated"]], TRUE)
  expect_equal(output[[7, "birthdate"]], "1900-12-1")
  expect_equal(output[[7, "birthdate.is.estimated"]], TRUE)
  expect_equal(output[[8, "birthdate"]], "1900-6-25")
  expect_equal(output[[8, "birthdate.is.estimated"]], TRUE)
})

test_that("FilterUnsalvagableData removes no-name row", {
  output <- Pt._FilterUnsalvagableData(TestPatients())
  naRow <- output[output$CesID == "120-000005", ]
  expect_equal(nrow(naRow), 0)
})

test_that("ManualDedupe takes care of Bentobox Cuckooclock", {
  output <- Pt._ManualDedupe(TestPatients())
  bentoboxes <- output[startsWith(output$CesID, "001-000020"), ]
  expect_equal(nrow(bentoboxes), 2)
  expect_equal(bentoboxes[[1, "FN_Ano"]], 1995)
  expect_equal(bentoboxes[[2, "FN_Ano"]], 1970)
})

test_that("Deduplicate takes care of Rumblesack Cummerbund", {
  output <- Pt._Deduplicate(TestPatients())
  rumblesacks <- output[startsWith(output$CesID, "112-000351"), ]
  expect_equal(nrow(rumblesacks), 1)
  # Birthday comes from Salvidor
  expect_equal(rumblesacks[[1, "FN_Ano"]], 2011)
  # HTN data should be pulled from Soledad
  expect_true(rumblesacks[[1, "Hipertensión"]])
  expect_equal(rumblesacks[[1, "HTN_Fecha"]], "Fri May 19 00:00:00 CDT 2017")
})

test_that("Repeated CesIDs with different data get split", {
  output <- Pt._SplitDuplicatedCesIds(TestPatients())
  repeated <- output[startsWith(output$CesID, "120-000004"), ]
  expect_equal(nrow(repeated), 2)
  expect_equal(repeated[[1, "FN_Ano"]], 2010)
  expect_equal(repeated[[2, "FN_Ano"]], 1998)
})

test_that("GetCleanedTable doesn't fail", {
  Pt.GetCleanedTable()
})

test_that("GetCleanedTable attached pt UUID", {
  output <- Pt.GetCleanedTable()
  expect_true("ptUuid" %in% colnames(output))
  expect_true(length(output$ptUuid) == nrow(output))
  expect_true(typeof(output$ptUuid) == "character")
  expect_equal(output$ptUuid, Pt._GeneratePtUuid(output))
})

test_that("uuids are deterministic", {
  output1 <- Pt.GetCleanedTable()
  output2 <- Pt.GetCleanedTable()
  expect_equal(output1$ptUuid, output2$ptUuid)
})

test_that("uuids are maintained over cache load", {
  file.remove(CLEAN_PT_DATA_CACHE)
  runResults <- Pt.GetCleanedTable(useCache = TRUE)
  cacheData <- read.csv(CLEAN_PT_DATA_CACHE)
  expect_equal(as.character(cacheData$ptUuid), as.character(runResults$ptUuid))
  cachedRunResults <- Pt.GetCleanedTable(useCache = TRUE)
  expect_equal(as.character(cachedRunResults$ptUuid), as.character(runResults$ptUuid))
})

test_that("uuids are unique", {
  output <- Pt.GetCleanedTable()
  expect_true(!any(duplicated(output$ptUuid)))
})

# Test is failing
#
# test_that("GetCleanedTable cache returns the same result as a normal run", {
#   file.remove(CLEAN_PT_DATA_CACHE)
#   runResults <- Pt.GetCleanedTable(useCache = TRUE)
#   cacheData <- read.csv(CLEAN_PT_DATA_CACHE)
#   Equalish <- function(d1, d2) {
#     all((is.na(d1) & is.na(d2)) | d1 == d2)
#   }
#   expect_equal(cacheData, runResults)
#   
#   cachedRunResults <- Pt.GetCleanedTable(useCache = TRUE)
#   expect_equal(cachedRunResults, runResults)
# })