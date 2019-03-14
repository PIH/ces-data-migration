#' prepare_program_enrollments
#' Prepares patient-program data for OpenMRS-Iniz
#'
#' Imports CSVs from `data/input/<site>/Pacientes.csv`, unless preproccessed
#' Patient data is available at `data/tmp/prepped-patients.csv`.
#' Uses that data to produce a CSV file at `ENROLLMENTS_OUTPUT_PATH`.
#' 
#' Data in Access that has no counterpart in OpenMRS is appended to the
#' consult note (which is coded as "Presenting history").
#' 
#' These output CSVs should then be imported by OpenMRS Initializer module.
#' 
#' @docType package
#' @name prepare_program_enrollments

library("tidyverse")

source("R/patient_util.R")
source("R/program_enrollment_util.R")

ENROLLMENTS_OUTPUT_PATH = "data/output/program_enrollments.csv"
USE_CLEAN_PT_DATA_CACHE <- TRUE

print("Getting patient data")
patients <- Pt.GetCleanedTable()

print("Producing program enrollment data")
enrollments <- PE.PrepareProgramEnrollments(patients)

print("Writing CSV")
write.csv(enrollments, ENROLLMENTS_OUTPUT_PATH,
  row.names = FALSE, na = ""
)

print("Done!")

