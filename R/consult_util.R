library("tidyverse")
source("R/util.R")

VITALS_ENCOUNTER_TYPE <- "Signes vitaux"
VITALS_CONSULT_COLS_TO_CONCEPTS <- c(
  "PA Sistólica" = "PIH:SYSTOLIC BLOOD PRESSURE",
  "PA Diastólica" = "PIH:DIASTOLIC BLOOD PRESSURE",
  "Sat de Oxigeno" = "PIH:BLOOD OXYGEN SATURATION",
  "FC" = "PIH:PULSE",
  "FR" = "PIH:RESPIRATORY RATE", 
  "Peso" = "PIH:WEIGHT (KG)",
  "Talla.con" = "PIH:HEIGHT (CM)",
  "Temperatura" = "PIH:TEMPERATURE (C)",
  "Glucosa" = "PIH:SERUM GLUCOSE"
)

CON_COLS_LOGICAL = c()
CON_COLS_CHAR = c()

#' GenerateEncounterUuid
#'
#' @param consults (data.frame): The consults data, flattened
#'
#' @return A UUID with dashes, unique per consult
Con._GenerateEncounterUuid <- function(consults, consultType) {
  Util.UuidHash(
    paste(
      consults$ConsID,
      consults$CESid,
      consults$Fecha,
      consults[["Hora de atención"]],
      consults$Nota,
      consultType
    )
  )
}

#' GenerateObsUuid
#'
#' @param consultObs (tbl) one row per obs, with col `obsType`
#'
#' @return A UUID with dashes, unique per consult-column
Con._GenerateObsUuid <- function(consults) {
  Util.UuidHash(
    paste(
      consults$ConsID,
      consults$CESid,
      consults$Fecha,
      consults[["Hora de atención"]],
      consults$Nota,
      consults$obsType
    )
  )
}

Con._Unfactorize <- function(consults) {
  for (col in CON_COLS_LOGICAL) {
    consults[col] %<>% sapply(as.logical)
  }
  for (col in CON_COLS_CHAR) {
    consults[col] %<>% sapply(as.character)
  }
  return(consults)
}

Con.GetConsults <- function() {
  communityPaths <- Util.CommunityPaths()
  consultsPerCommunity <- map(paste0(communityPaths, "/Consultas.csv"),
                    read_csv)
  consultsPerCommunity %<>% Util.AppendClinicNames(Util.CommunityPaths())
  consultsFlat <- do.call(rbind, consultsPerCommunity)
  consults <- Con._Unfactorize(consultsFlat)
  return(consults)
}

#' PrepareGeneralConsultData
#'
#' @param consults : raw, flattened consults from consultsPerCommunity
#' @param patients : cleaned patients table from Pat.GetCleanedTable()
#'
#' @return consults, with a bunch of additional columns not specific to
#'           a particular type of encounter or observation
#' @export
Con.PrepareGeneralConsultData <- function(consults, patients) {
  # Drop rows with no CESid
  consults <- consults[!is.na(consults["CESid"]) & consults["CESid"] != "", ]
  
  consults <- inner_join(consults, patients,
                         by = c("CESid" = "CesID"),
                         suffix = c(".con", ".pt"))
  consultDate <- Util.TransformDate(consults$Fecha)  # `Hora de atención` has no valid data
  consults <- bind_cols(consults,
                        consultDate = consultDate)
  return(consults)
}

#' PrepareVitalsData
#'
#' @param consults : the prepped consults table from Con.PrepareGeneralConsultData()
#'
#' @return a named list containing two tbls, encounters and obs
#' @export
Con.PrepareVitalsData <- function(consults) {
  
  vitalsConsults <- consults[
    Util.AnyEntryNotNa(consults,
                       names(VITALS_CONSULT_COLS_TO_CONCEPTS)
    ),
  ]
  
  vitalsConsults <- add_column(vitalsConsults,
    encUuid = Con._GenerateEncounterUuid(vitalsConsults, VITALS_ENCOUNTER_TYPE))
  
  encounters <- tibble(
    "uuid" = vitalsConsults$encUuid,
    "Void/Retire" = FALSE,
    "Date" = vitalsConsults$consultDate,
    "Patient UUID" = vitalsConsults$ptUuid,
    "Location" = vitalsConsults$commName.con,
    "Encounter Type" = VITALS_ENCOUNTER_TYPE
  )
  
  # create tbl with one row per non-NA obs
  consultObs <- vitalsConsults %>%
    gather(key = "obsType", value = "obsValue",
           names(VITALS_CONSULT_COLS_TO_CONCEPTS),
           na.rm = TRUE)
  
  consultObs <- add_column(consultObs,
                           obsUuid = Con._GenerateObsUuid(consultObs))
  
  consultObs <- mutate(consultObs,
                       concept = VITALS_CONSULT_COLS_TO_CONCEPTS[obsType])
  
  obs <- tibble(
    "uuid" = consultObs$obsUuid,
    "Void/Retire" = FALSE,
    "Person UUID" = consultObs$ptUuid,
    "Location" = consultObs$commName.con,
    "Encounter UUID" = consultObs$encUuid,
    "Concept" = consultObs$concept,
    "Value" = consultObs$obsValue,
    "Set Members" = NA,
    "Set Member Values" = NA
  )
  
  res <- list(
    encounters = encounters,
    obs = obs
  )
  return(res)
}

#' PrepareConsultObsData
#'
#' @param consults : the prepped consults table from Con.PrepareConsultData()
#'
#' @return a table of observation data, one row per obs
#' @export
Con.PrepareConsultObsData <- function(consults) {
  
}
