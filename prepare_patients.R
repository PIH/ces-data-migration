#' prepare_patients
#' Converts patient data from Access format to OpenMRS-Iniz
#'
#' Imports CSVs from `data/input/<site>/Pacientes.csv`. Uses that data to produce
#' a CSV file, `data/output/patients.csv`, which should be imported by OpenMRS
#' Initializer module.
#' 
#' Deduplicates patients with identical CesID, Nombre, and Apellido. If those
#' patients have different data, issues a warning. You should use that warning
#' to add that patient to `R/patient_util.R:ManualDedupe`, which splits these
#' patient entries instead of merging them. The duplicates handled by
#' `ManualDedupe` get `-1`, `-2`, etc. appended to their CesID.
#' 
#' Splits patients with identical CesID but different name or birthdate. Each
#' distinct patient is assigned a new CesID with suffix `-1`, `-2`, etc.
#' 
#' The logic for this script is almost entirely contained in `R/patient_util.R`.
#' 
#' @docType package
#' @name prepare_patients

library("tidyverse")

source("R/util.R")
source("R/patient_util.R")

USE_CLEAN_PT_DATA_CACHE <- FALSE
PT_OUTPUT_PATH = "data/output/patients.csv"

tmp.patients <- Pt.GetCleanedTable()

print("Preparing output data")
output.patients <- Pt.PrepareOutputData(tmp.patients)

print("Writing CSV")
write.csv(output.patients, PT_OUTPUT_PATH,
  row.names = FALSE, na = ""
)

print("Done!")
