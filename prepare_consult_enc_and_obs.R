#' prepare_consult_enc_and_obs
#' Prepares consult data for OpenMRS-Iniz
#'
#' Imports CSVs from `data/input/<site>/Consultas.csv`. Uses that data to produce
#' two CSV files for each encounter type.
#' 
#' - `data/output/encounters-*.csv` contains the consult encounters, one line per patient.
#' - `data/output/obs-*.csv` contains all the observations for each of those consults.
#' 
#' Data in Access that has no counterpart in OpenMRS is appended to the
#' consult note (which is coded as "Presenting history").
#' 
#' These output CSVs should then be imported by OpenMRS Initializer module.
#' 
#' @docType package
#' @name prepare_consult_enc_and_obs

library("tidyverse")
source("R/util.R")
source("R/patient_util.R")
source("R/consult_util.R")

BASE_OUTPUT_PATH = "data/output/"
USE_CLEAN_PT_DATA_CACHE <- TRUE

WriteResults <- function(res, name) {
  encOutputPath <- paste(BASE_OUTPUT_PATH, "encounters-", name, ".csv", sep = "")
  print(paste("Writing encounters CSV to", encOutputPath))
  write_csv(res$encounters, encOutputPath, na = "")
  
  obsOutputPath <- paste(BASE_OUTPUT_PATH, "obs-", name, ".csv", sep = "")
  print(paste("Writing obs CSV to", obsOutputPath))
  write_csv(res$obs, obsOutputPath, na = "")
}

patients <- Pt.GetCleanedTable()
print("Getting consult data")
rawConsults <- Con.GetConsults()
print("Doing general consult data prep")
consults <- Con.PrepareGeneralConsultData(rawConsults, patients)
print("Prepping vitals data")
vitalsRes <- Con.PrepareVitalsData(consults)
WriteResults(vitalsRes, "vitals")
print("Prepping consults data")
consultFormRes <- Con.PrepareConsultFormData(consults)
WriteResults(consultFormRes, "consults")


print("Done!")