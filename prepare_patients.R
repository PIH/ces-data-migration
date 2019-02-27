#' prepare_patients: Converts patient data from Access format to OpenMRS-Iniz
#'
#' Imports CSVs from data/input/<site>/Pacientes.csv. Uses that data to produce
#' a CSV file, data/output/patients.csv, which should be imported by OpenMRS
#' Initializer module.
#' 
#' Deduplicates patients with identical CesID, Nombre, and Apellido. If those
#' patients have different data, issues a warning. You should use that warning
#' to add that patient to `R/patient_util.R:ManualDedupe`, which splits these
#' patient entries instead of merging them. The duplicates handled by
#' `ManualDedupe` get `-1`, `-2`, etc. appended to their CesID.
#' 
#' @docType package
#' @name prepare_patients

library("tidyverse")

source("R/util.R")
source("R/patient_util.R")

ParseIdentifier <- function(oldId, location) {
  paste("Old Identification Number", oldId, location, sep = ":")
}

CesIdentifier <- function(index, location) {
  prefix <- stringr::str_to_upper(substr(location, 1, 3))
  idNum <- 1000000 + index
  id <- paste0(prefix, idNum)
  paste("Chiapas EMR ID", id, location, sep = ":")
}

ParseGender <- function(g) {
  ifelse(is.na(g), "U", ifelse(g == "1", "F", "M"))
}

ParseAddresses <- function(addr) {
  paste("cityVillage", addr, sep = ":")
}

# Main #########################################################################

tmp.patients <- Pt.GetCleanedTable()

print("Extracting attributes")
identifiers <- paste(
  ParseIdentifier(unlist(tmp.patients["CesID"]), tmp.patients[["commName"]]),
  CesIdentifier(1:dim(tmp.patients)[1], tmp.patients[["commName"]]),
  sep = ";"
)

print("Constructing output data")
output.patients <- data.frame(
  "uuid" = Pt.GeneratePtUuid(tmp.patients),
  "Identifiers" = identifiers,
  "Given names" = tmp.patients[["Nombre"]],
  "Family names" = tmp.patients[["Apellido"]],
  "Gender" = ParseGender(tmp.patients[["Sexo"]]),
  "Birthdate" = tmp.patients[["birthdate"]],
  "Date created" = tmp.patients[["createdDate"]],
  "Addresses" = ParseAddresses(tmp.patients[["Comunidades"]]),
  "Void/Retire" = FALSE,
  check.names = FALSE # allow column names to have spaces
)

print("Writing CSV")
write.csv(output.patients, "data/output/patients.csv",
  row.names = FALSE, na = ""
)

print("Done!")
