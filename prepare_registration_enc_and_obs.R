#' prepare_registration_enc_and_obs
#' Prepares registration data for OpenMRS-Iniz
#'
#' Imports CSVs from `data/input/<site>/Pacientes.csv`. Uses that data to produce
#' two CSV files. `ENC_OUTPUT` contains the registration encounters, one line per
#' patient. `OBS_OUTPUT` contains all the observations, of which there will be 3-5
#' per patient.
#' 
#' These output CSVs should then be imported by OpenMRS Initializer module.
#' 
#' @docType package
#' @name prepare_registration_enc_and_obs

library("tidyverse")
source("R/util.R")
source("R/patient_util.R")

ENC_OUTPUT_PATH = "data/output/encounters_registration.csv"
OBS_OUTPUT_PATH = "data/output/obs_registration.csv"
ENCOUNTER_TYPE_NAME = "Enregistrement de patient"
USE_CLEAN_PT_DATA_CACHE <- TRUE

#' GenerateRegEncounterUuid
#'
#' @param ptsDf (data.frame): The patient data
#'
#' @return A UUID with dashes, unique per patient, different from the pt UUID
GenerateRegEncounterUuid <- function(ptsDf) {
  Util.UuidHash(
    paste(
      unlist(ptsDf["Nombre"]),
      unlist(ptsDf["Apellido"]),
      unlist(ptsDf["Fechar.de.registro"]),
      unlist(ptsDf["CesID"])
    )
  )
}

GenerateRegObsUuid <- function(ptsDf, colName) {
  Util.UuidHash(
    paste(
      unlist(ptsDf["Nombre"]),
      unlist(ptsDf["Apellido"]),
      unlist(ptsDf["Fechar.de.registro"]),
      unlist(ptsDf["CesID"]),
      colName
    )
  )
}

# Import data
tmp.patients <- Pt.GetCleanedTable()

encUuids = GenerateRegEncounterUuid(tmp.patients)
locations = tmp.patients[["commName"]]


# Output Encounters ############################################################

dates = Util.TransformDate(unlist(tmp.patients["Fechar.de.registro"]))
dates[is.na(dates)] <- "2000-01-01T00:00:00Z"
output.encounters <- data.frame(
  "uuid" = encUuids,
  "Void/Retire" = FALSE,
  "Date" = dates,
  "Patient UUID" = tmp.patients$ptUuid,
  "Location" = locations,
  "Encounter Type" = ENCOUNTER_TYPE_NAME,
  check.names = FALSE # allow column names to have spaces
)

write.csv(output.encounters, ENC_OUTPUT_PATH,
  row.names = FALSE, na = ""
)


# Output Obs ###################################################################

SpConceptForBoolean <- function(value) {
  ifelse(value, "PIH:Seguro Popular", NA)  # NA rows will be filtered out
}

tmp.obsConcepts <- data.frame(
  "indig" = "PIH:Indigenous",
  "migrant" = "PIH:Immigrant",
  "disc" = "CIEL:162558"
)

tmp.obsStructs <- data.frame(
  "prospera" = c("PIH:Prospera Construct", "PIH:Has Prospera"),
  "sp" = c("PIH:Insurance construct", "PIH:Mexico Insurance Coded")
)

tmp.obsValues <- data.frame(
  "indig" = Util.YesNoConceptForBoolean(unlist(tmp.patients["Indígena"])),
  "migrant" = Util.YesNoConceptForBoolean(unlist(tmp.patients["Migrante"])),
  "disc" = Util.YesNoConceptForBoolean(unlist(tmp.patients["Discapacidad"])),
  "prospera" = Util.YesNoConceptForBoolean(unlist(tmp.patients["Oportunidades"])),
  "sp" = SpConceptForBoolean(unlist(tmp.patients["SPSS"]))
)

output.obs <- NULL

for (type in c("indig", "migrant", "disc")) {
  obsUuids = GenerateRegObsUuid(tmp.patients, type)
  dataForType <- data.frame(
    "uuid" = obsUuids,
    "Void/Retire" = FALSE,
    "Person UUID" = tmp.patients$ptUuid,
    "Location" = locations,
    "Encounter UUID" = encUuids,
    "Concept" = unname(tmp.obsConcepts[type]),
    "Value" = unname(tmp.obsValues[type]),
    "Set Members" = NA,
    "Set Member Values" = NA,
    check.names = FALSE # allow column names to have spaces
  )
  output.obs <- rbind(output.obs, dataForType)
}

for (type in c("prospera", "sp")) {
  obsUuids = GenerateRegObsUuid(tmp.patients, type)
  dataForType <- data.frame(
    "uuid" = obsUuids,
    "Void/Retire" = FALSE,
    "Person UUID" = tmp.patients$ptUuid,
    "Location" = locations,
    "Encounter UUID" = encUuids,
    "Concept" = tmp.obsStructs[[type]][1],
    "Value" = NA,
    "Set Members" = tmp.obsStructs[[type]][[2]],
    "Set Member Values" = tmp.obsValues[[type]],
    check.names = FALSE # allow column names to have spaces
  )
  filteredDataForType <- dataForType[!is.na(dataForType["Set Member Values"]), ]
  output.obs <- rbind(output.obs, filteredDataForType)
}

write.csv(output.obs, OBS_OUTPUT_PATH,
  row.names = FALSE, na = ""
)