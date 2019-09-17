library("tidyverse")
source("R/util.R")
source("R/consult_util.R")  # CONSULT_FORM_ENCOUNTER_TYPE, uuid fcns
source("R/dx_map.R")  # DX_MAP

Dx._GenerateObsUuid <- function(diagnoses) {
  result <- Util.UuidHash(
    paste(
      diagnoses$encUuid,
      diagnoses$Diagnostico
    )
  )
  return(result)
}

Dx._DiagnosisMapper <- function(x) {
  if (!is.na(x)) {
    charX <- as.character(x)
    if (charX %in% names(DX_MAP)) {
      return(
        list(
          obsValue = NA,
          setMembers = paste(
            "PIH:DIAGNOSIS",
            "PIH:CLINICAL IMPRESSION DIAGNOSIS CONFIRMED",
            "PIH:Diagnosis order",
            sep = ";"
          ),
          setValues = paste(
            DX_MAP[[charX]],
            "PIH:PRESUMED",
            "PIH:primary",
            sep = ";"
          )
        )
      )
    }
  }
  return(NA)
}


Dx.GetDiagnoses <- function() {
  communityPaths <- Util.CommunityPaths()
  dxsPerCommunity <- map(
    paste0(communityPaths, "/Diagnosticos.csv"),
    read_csv
  )
  dxsPerCommunity %<>% Util.AppendClinicNames(Util.CommunityPaths())
  dxsFlat <- do.call(rbind, dxsPerCommunity)
  dxs <- Con._Unfactorize(dxsFlat)
  return(dxs)
}

#' @param diagnoses : raw, flattened diagnoses from Con.GetDiagnoses
#' @param consults : output from Con.PrepareGeneralConsultData
Dx.PrepareDiagnosisData <- function(rawDiagnoses, consults) {
  consults <- select(consults, "ConsID", "Fecha", "CESid", "Nota", "consultDate", "ptUuid", "commName.con")
  vprint("..Generate encounter UUID")
  encUuids <- Con._GenerateEncounterUuid(consults, CONSULT_FORM_ENCOUNTER_TYPE)
  consults <- add_column(consults, encUuid = encUuids)
  vprint("..Join Consultas with Diagnosticos")
  diagnoses <- inner_join(consults, rawDiagnoses,
    by = c("ConsID" = "ConsID", "commName.con" = "commName"),
    suffix = c(".con", ".dx")
  )
  vprint("..Generate Obs UUID")
  obsUuids <- Dx._GenerateObsUuid(diagnoses)
  diagnoses <- add_column(diagnoses, obsUuid = obsUuids)

  vprint("..Map diagnoses to new values")
  obsValData <- map(diagnoses$Diagnostico, Dx._DiagnosisMapper)
  diagnoses <- filter(diagnoses, !is.na(obsValData))  # filter
  obsValData <- obsValData[!is.na(obsValData)]          # filter
  obsValDataNames <- names(obsValData[[1]])
  obsValData <- lapply(obsValData, as.character)  # this kills the names
  # Put the names back
  obsValData <- lapply(obsValData, function(l) { names(l) <- obsValDataNames; return(l); })
  obsValData <- bind_rows(lapply(obsValData, as.data.frame.list))  # convert list of vectors to df
  names(obsValData) <- obsValDataNames
  diagnoses <- bind_cols(diagnoses, obsValData)
  # Filter out bad pt ids
  diagnoses <- filter(diagnoses, ptUuid != Util.UuidHash(""))
  return(diagnoses)
}

Dx.PrepareDiagnosisObs <- function(diagnosisObs) {
  tibble(
    "uuid" = diagnosisObs$obsUuid,
    "Void/Retire" = FALSE,
    "Person UUID" = diagnosisObs$ptUuid,
    "Location" = diagnosisObs$commName.con,
    "Encounter UUID" = diagnosisObs$encUuid,
    "Concept" = "PIH:Visit Diagnoses",
    "Value" = NA,
    "Set Members" = diagnosisObs$setMembers,
    "Set Member Values" = diagnosisObs$setValues
  )
}
