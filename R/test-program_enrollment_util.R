library("testthat")
library("tidyverse")
source("R/patient_util.R")
source("R/program_enrollment_util.R")

VERBOSE = FALSE

# override with test data
CLEAN_PT_DATA_CACHE <- "data/test/patient-cache.csv"
Util.CommunityPaths <- function() {
  list.dirs("data/test")[-1] # element 1 is "data/test" itself
}

test_that("PrepareProgramEnrollments produces expected output", {
  patients <- Pt.GetCleanedTable()
  output <- PE.PrepareProgramEnrollments(patients)
  expect_equal(2, nrow(output))
  dm_guy <- output[output$`Person UUID` == "48ebb1ba-b628-d481-7f09-296d30870470", ]
  expect_equal(1, nrow(dm_guy))
  expect_equal("Diabetes", dm_guy$`Program Name`)
  htn_guy <- output[output$`Person UUID` == "b5c8344e-7c0c-6f88-b94c-9da3f85018bf", ]
  expect_equal(1, nrow(htn_guy))
  expect_equal("Hypertension", htn_guy$`Program Name`)
  expect_equal("2017-05-18T19:00:00-0500", htn_guy$`Date Enrolled`)
})

test_that("uuids are unique", {
  patients <- Pt.GetCleanedTable()
  output <- PE.PrepareProgramEnrollments(patients)
  expect_true(!any(duplicated(output$peUuid)))
})
