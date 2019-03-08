# NB: You don't have to deduplicate rows -- if two output rows have the same
# UUID, Iniz will treat them as referring to the same object. The first time
# it encounters the UUID it will add the object; subsequently it will update it.


# Notes on the data:
# Neither `Hora de atención` nor `Hora de llegada` have valid data

library("tidyverse")
source("R/util.R")

# Globals and Specs ############################################################

id <- function(x) x
codedYes <- function(x) ifelse(x, "PIH:YES", NA)
codedNo <- function(x) ifelse(x, "PIH:NO", NA)
coded12ReactiveText <- function(x) ifelse(is.na(x), NA, switch(x, "Reactivo", "No reactivo"))

# Here we define the encounter types and "mapping specs."
# A mapping spec has three columns:
#   1. The Access column header
#   2. The concept reference code
#   3. A function mapping the old value to the new one

VITALS_ENCOUNTER_TYPE <- "Signes vitaux"
VITALS_CONSULT_MAPPING_SPEC <- tribble(
  ~"accessCol", ~"concept", ~"valueMapper",
  "PA Sistólica", "PIH:SYSTOLIC BLOOD PRESSURE", id,
  "PA Diastólica", "PIH:DIASTOLIC BLOOD PRESSURE", id,
  "Sat de Oxigeno", "PIH:BLOOD OXYGEN SATURATION", id,
  "FC", "PIH:PULSE", id,
  "FR", "PIH:RESPIRATORY RATE",  id,
  "Peso", "PIH:WEIGHT (KG)", id,
  "Talla.con", "PIH:HEIGHT (CM)", id,
  "Temperatura", "PIH:TEMPERATURE (C)", id,
  "Glucosa", "PIH:SERUM GLUCOSE", id,
)

CONSULT_FORM_ENCOUNTER_TYPE <- "Mexico Consult"
CONSULT_FORM_SPEC <- tribble(
  ~"accessCol", ~"concept", ~"valueMapper",
  "Nota", "CIEL:1390", id,  # concept "presenting history"
  # asthma
  "Síntomas NocturnosSI", "PIH:FUNCTIONAL REVIEW OF SYMPTOMS CONSTRUCT", codedYes,
  "Limitación de la ActividadSI", "PIH:Limitation of ability to perform main daily activities coded", codedYes,
  "Medicamento de rescateSI", "PIH:Medications more that twice per week", codedYes,
  "Síntomas NocturnosNO", "PIH:FUNCTIONAL REVIEW OF SYMPTOMS CONSTRUCT", codedNo,
  "Limitación de la ActividadNO", "PIH:Limitation of ability to perform main daily activities coded", codedNo,
  "Medicamento de rescateNO", "PIH:Medications more that twice per week", codedNo,
  # diabetes
  "Glucosa", "PIH:SERUM GLUCOSE", id,
  "Colesterol", "PIH:TOTAL CHOLESTEROL", id,
  "HDL", "PIH:HIGH-DENSITY LIPOPROTEIN CHOLESTEROL", id,
  "LDL", "PIH:LOW-DENSITY LIPOPROTEIN CHOLESTEROL", id,
  # epilepsy
  "Número de ataques", "PIH:Number of seizures in the past month", id,
  # mental
  "PHQ-9", "CIEL:165137", id
)

# Here we define a specification for columns that don't yet or won't exist
# in the new system, to be appended to the clinical note of the corresponding
# encounter.
CONSULT_NOTE_APPENDS <- tribble(
  ~"accessCol", ~"friendlyName", ~"valueMapper",
  "PeakflowSI", "Asma: Peakflow", id,
  "Efectos Secundarios AD", "Efectos secundarios de antidepresivos", id,
  "Framingham", "DM: Framingham", id,
  "Examen de Orina", "DM: Examen de Orina", id,
  "VIH", "Emb: VIH", coded12ReactiveText,
  "VDRL", "Emb: VDRL", coded12ReactiveText, 
  "zscore", "Emb: zscore", id, 
  "Frecuencia cardiaca fetal", "Emb: Frecuencia cardiaca fetal", id, 
  "Fondo uterino", "Emb: Fondo uterino", id, 
  "Semanas de Gestación", "Emb: Semanas de Gestación", id, 
  "FPP.con", "Emb: FPP.con", id, 
  "FUM.con", "Emb: FUM.con", id, 
  "Hemglobina", "Emb: Hemoglobina", id, 
  "EGO", "Emb: EGO", id
)

# Notes on other columns not mapped above:
# Mysteries:
#   Referido (179 data points)
#   Contrareferido (8 data points)
# Discard:
#   No data: HBA1C, Trimestre, CESidemb, AR, BR, Gesta, Para, Aborto, Cesárea,
#       FPP.pt, FUM.pt, TDRef.*, TD*.*, Función pulmonarSI 
#   A1c has a few data points but seems to have 0 as default rather than NA
#   Pap_smear seems to have a handful of GAD-7 scores, a date in 2014, and nothing else
#   Revisión de Pies doesn't map in any reasonable way to new structure
#       (in Access it's boolean done-or-not, in OpenMRS it's coded with info)
#   Not in new system:
#     PeakflowSI (36 data points)
#     Efectos Secundarios AD (AD is antidepressants; 49 data points)


# Internal / Utility Functions #################################################

#' GenerateEncounterUuid
#'
#' @param consults (data.frame): The consults data, flattened
#'
#' @return A UUID with dashes, unique per consult
Con._GenerateEncounterUuid <- function(consults, consultType) {
  result <- Util.UuidHash(
    paste(
      consults$ConsID,
      consults$CESid,
      consults$Fecha,
      consults$Nota,
      consultType
    )
  )
  return(result)
}

#' GenerateObsUuid
#'
#' @param consultObs (tbl) one row per obs, with col `obsType`
#'
#' @return A UUID with dashes, unique per consult-column
Con._GenerateObsUuid <- function(consults) {
  result <- Util.UuidHash(
    paste(
      consults$ConsID,
      consults$CESid,
      consults$Fecha,
      consults$Nota,
      consults$accessCol
    )
  )
  return(result)
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

#' PrepareOutputEncAndSimpleObs
#'
#' @param consults 
#' @param mappingSpec 
#' @param encounterType 
#' @param appendSpec (NULL | tbl) default NULL. If specified, should be tbl
#'   with cols `accessCol`, `concept`, `valueMapper`. If specified,
#'   data in `consults[accessCol]` will be appended to `consults$Nota`
#'
#' @return
#' @export
#'
#' @examples
Con._PrepareOutputEncAndSimpleObs <- function(consults,
                                              mappingSpec,
                                              encounterType,
                                              appendSpec = NULL) {
  
  specifiedConsults <- consults[
    Util.AnyEntryNotNa(consults,
                       mappingSpec[[1]]
    ),
  ]
  
  encUuids <- Con._GenerateEncounterUuid(specifiedConsults, encounterType)
  specifiedConsults <- add_column(specifiedConsults, encUuid = encUuids)
  encounters <- tibble(
    "uuid" = specifiedConsults$encUuid,
    "Void/Retire" = FALSE,
    "Date" = specifiedConsults$consultDate,
    "Patient UUID" = specifiedConsults$ptUuid,
    "Location" = specifiedConsults$commName.con,
    "Encounter Type" = encounterType
  )
  
  # Append uncoded data to consult note if an append spec is present
  if (!is.null(appendSpec)) {
    specifiedConsults <- Con._AttachUncodedColsToConsultNote(specifiedConsults, appendSpec)
  }
  
  # create tbl with one row per non-NA obs
  consultObs <- specifiedConsults %>%
    gather(key = "accessCol", value = "accessVal",
           mappingSpec[[1]],
           na.rm = TRUE)
  
  # Add obsUuid
  obsUuids <- Con._GenerateObsUuid(consultObs)
  consultObs <- add_column(consultObs, obsUuid = obsUuids)
  
  # Add obsValue
  consultObs <- merge(consultObs, mappingSpec, by = "accessCol")
  obsValues <- invoke_map_chr(consultObs$valueMapper, consultObs$accessVal)
  consultObs <- add_column(consultObs, obsValue = obsValues)
  
  # Filter NA values
  consultObs <- filter(consultObs, !is.na(obsValue))
  
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

Con._AttachUncodedColsToConsultNote <- function(consults, appendSpec) {
  appendString <- Con._ProduceUncodedColString(consults, appendSpec)
  consults$Nota <- map2_chr(consults$Nota, appendString,
    function(note, appendStr) {
      ifelse(appendStr != "", paste(note, appendStr, sep = "\n\n"), note)
  })
  return(consults)
}

Con._ProduceUncodedColString <- function(consults, appendSpec) {
  values <- consults[appendSpec$accessCol]
  valTbl <- pmap(appendSpec, function(accessCol, friendlyName, valueMapper) {
    colValues <- invoke_map_chr(valueMapper,
                                values[[accessCol]])
    colValues <- ifelse(!is.na(as.numeric(colValues)),
                        prettyNum(as.numeric(colValues)),
                        colValues)
    colValues <- map_chr(colValues, function(v) {
      ifelse(is.na(v),
             NA,
             paste(friendlyName, v, sep = ": ", collapse = "\n")
      )
    })
    return(colValues)
  })
  names(valTbl) <- appendSpec$accessCol
  valTbl <- as_tibble(valTbl)
  values <- apply(valTbl, 1, function(r) { paste(na.omit(r), collapse = "\n") })
  return(values)
}

# Interface Functions ##########################################################

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
  consultDate <- Util.TransformDate(consults$Fecha)
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
  Con._PrepareOutputEncAndSimpleObs(consults,
    VITALS_CONSULT_MAPPING_SPEC,
                                    VITALS_ENCOUNTER_TYPE
                                    )
}

#' PrepareConsultFormData
#' 
#' Produces the encounter and obs data corresponding to the Mexico Consult Form,
#' except the Diagnosis and Drug fields.
#'
#' @param consults : the prepped consults table from Con.PrepareConsultData()
#'
#' @return a named list containing two tbls, encounters and obs
#' @export
Con.PrepareConsultFormData <- function(consults) {
  Con._PrepareOutputEncAndSimpleObs(consults,
    CONSULT_FORM_SPEC,
                                              CONSULT_FORM_ENCOUNTER_TYPE,
    CONSULT_NOTE_APPENDS
    
                                              )
}
