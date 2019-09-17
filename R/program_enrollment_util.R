library("tidyverse")
source("R/util.R")

PROGRAM_SPEC <- tribble(
  ~"accessCol",   ~"programName",  ~"dateCol",
  "Asma",         "Asthma",        "AM_Fecha",
  "Depresión",    "Mental Health", "DEP_Fecha",
  "Desnutrición", "Malnutrition",  "DES_Fecha",
  "Diabetes",     "Diabetes",      "DM_Fecha",
  "Embarazada",   "ANC",           "EMB_Fecha",
  "Epilepsia",    "Epilepsy",      "EP_Fecha",
  "Hipertensión", "Hypertension",  "HTN_Fecha"
)

#' GeneratePeUuid
#'
#' @param programEntries (tbl) patient data with accessCol column and
#'   one line per program enrollment
#'
#' @return
PE._GeneratePeUuid <- function(programEntries) {
  Util.UuidHash(
    paste(
      programEntries$CesID,
      programEntries$accessCol
    )
  )
}

#' PrepareProgramEnrollments
#'
#' @param patients : the prepped patient data from Pt.GetCleanedTable()
#'
#' @return (tbl) enrollments in the format expected by Iniz
#' @export
PE.PrepareProgramEnrollments <- function(patients) {
  patientsInAnyProgram <- patients[
    Util.AnyEntryTrue(
      patients,
      PROGRAM_SPEC[[1]]
    ),
  ]
  
  # create tbl with one row per program entry
  programEntriesUnfiltered <- patientsInAnyProgram %>%
    gather(
      key = "accessCol", value = "accessVal",
      PROGRAM_SPEC[[1]]
    )
  
  # Add date
  programEntriesUnfilteredDates <- patientsInAnyProgram %>%
    gather(
      key = "dateColName", value = "rawEnrollDate",
      PROGRAM_SPEC[[3]]
    )
  
  programEntriesUnfiltered <- bind_cols(
    programEntriesUnfiltered,
    programEntriesUnfilteredDates["rawEnrollDate"]
  )
  
  programEntriesUnfiltered <- mutate(
    programEntriesUnfiltered,
    enrollDate = Util.TransformDate(rawEnrollDate)
  )
  
  programEntries <- programEntriesUnfiltered[programEntriesUnfiltered$accessVal, ]
  
  # Filter out old pregnancies
  programEntries <- programEntries[programEntries$accessCol != "Embarazada" | programEntries$enrollDate > lubridate::ymd("2018-06-01"), ]
  
  # Add peUuid
  peUuid <- PE._GeneratePeUuid(programEntries)
  programEntries <- add_column(programEntries, peUuid = peUuid)

  # Add entries for program name & date column name
  programEntries <- inner_join(programEntries, PROGRAM_SPEC, by = "accessCol")
  
  pe <- tibble(
    "uuid" = programEntries$peUuid,
    "Void/Retire" = FALSE,
    "Person UUID" = programEntries$ptUuid,
    "Location" = programEntries$commName,
    "Program Name" = programEntries$programName,
    "Date Enrolled" = programEntries$enrollDate,
    "Date Completed" = NA,
    "Outcome Concept" = NA
  )

  return(pe)
}