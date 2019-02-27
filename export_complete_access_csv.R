#' export_complete_access_csv: Exports a giant CSV with all pts and consults
#'
#' Imports CSVs from data/input/<site>/. Uses that data to produce
#' a giant CSV file. One line per consult.
#' 
#' @docType package
#' @name export_complete_access_csv

library("tidyverse")
source("R/patient_util.R")
source("R/consult_util.R")

EXPORT_PATH = "data/output/complete_access_data.csv"

patients <- Pt.GetCleanedTable(useCache = TRUE)
print("Getting consult data")
rawConsults <- Con.GetConsults()
print("Prepping consult data")
consults <- Con.PrepareGeneralConsultData(rawConsults, patients)

print(paste("Writing giant CSV to", EXPORT_PATH))
write.csv(consults, EXPORT_PATH,
          row.names = FALSE, na = ""
)


