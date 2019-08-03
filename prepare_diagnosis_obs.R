#' prepare_diagnosis_obs
#' Prepares diagnosis data for OpenMRS-Iniz
#'
#' Imports CSVs from `data/input/<site>/Pacientes.csv`, unless preproccessed
#' Patient data is available at `data/tmp/prepped-patients.csv`. Also pulls
#' data from `data/input/<site>/Consultas.csv` and
#' `data/input/<site>/Diagnosticos.csv`.
#' 
#' This output CSV should then be imported by OpenMRS Initializer module.
#' 
#' @docType package
#' @name prepare_diagnosis_obs

library("tidyverse")

source("R/patient_util.R")
source("R/consult_util.R")
source("R/diagnosis_util.R")

DX_OUTPUT_PATH = "data/output/obs-dx.csv"
USE_CLEAN_PT_DATA_CACHE <- TRUE

print("Getting patient data")
patients <- Pt.GetCleanedTable()

print("Getting consult data")
rawConsults <- Con.GetConsults()

print("Doing general consult data prep")
cachedConsults <- Con.PrepareGeneralConsultData(rawConsults, patients)
consults <- cachedConsults

print("Getting diagnosis data")
rawDiagnoses <- Dx.GetDiagnoses()

print("Prepping diagnosis data")
diagnoses <- Dx.PrepareDiagnosisData(rawDiagnoses, consults)

vprint("..Deduplicate")
dedupedDiagnoses <- diagnoses[!duplicated(diagnoses$obsUuid), ]

print("Prepping diagnosis obs")
dxObs <- Dx.PrepareDiagnosisObs(dedupedDiagnoses)

print("Writing CSV")
write.csv(dxObs, DX_OUTPUT_PATH,
  row.names = FALSE, na = ""
)

print("Done!")

