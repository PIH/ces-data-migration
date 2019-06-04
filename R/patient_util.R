library("tidyverse")
library("magrittr")
source("R/util.R")

VERBOSE <- TRUE

CLEAN_PT_DATA_CACHE <- "data/tmp/prepped-patients.csv"
USE_CLEAN_PT_DATA_CACHE <- FALSE
  
PT_COLS_LOGICAL <- c("Actualmente.embarazada", "SPSS", "Oportunidades",
                    "Migrante", "Indígena", "Discapacidad", "SUIVE", "Diabetes",
                    "Hipertensión", "Asma", "Embarazada", "Depresión",
                    "Epilepsia", "Desnutrición")

PT_COLS_CHAR <- c("CesID", "Nombre", "Apellido", "AM_Fecha", "DEP_Fecha",
                  "DM_Fecha", "EMB_Fecha", "EP_Fecha", "HTN_Fecha",
                  "DES_Fecha")

vcat <- function(content) {
  if (VERBOSE) {
    cat(content)
  }
}

vprint <- function(content) {
  if (VERBOSE) {
    print(content)
  }
}

# Data Prep Helpers ############################################################

Pt._Unfactorize <- function(patients) {
  for (col in PT_COLS_LOGICAL) {
    patients[col] %<>% sapply(as.logical)
  }
  for (col in PT_COLS_CHAR) {
    patients[col] %<>% sapply(as.character)
  }
  return(patients)
}

Pt._FixDatatypes <- function(patients) {
  for (col in PT_COLS_LOGICAL) {
    patients[[col]] <- Util.ConvertTextToBoolVector(patients[[col]])
  }
  return(patients)
}

# Replaces blank names with "-"
Pt._FixInvalidData <- function(patients) {
  patients$Nombre[patients$Nombre == ""] <- "-"
  patients$Apellido[patients$Apellido == ""] <- "-"
  return(patients)
}

# Expects that blank names have been replaced by "-"
Pt._FilterUnsalvagableData <- function(patients) {
  patients <- patients[
    !(patients$Nombre == "-" & patients$Apellido == "-"),
  ]
  return(patients)
}

#' GeneratePtUuid
#'
#' Constructs patient UUIDs from CesID. This implicitly requires that each
#' patient has a unique CesID. CesIDs should therefore be cleaned and
#' deduplicated before UUIDs are calculated.
#'
#' @param The whole patients dataframe
#'
#' @return A UUID string with dashes
#'
Pt._GeneratePtUuid <- function(table) {
  Util.UuidHash(table$CesID)
}

#' DenormalizeCommunityNames
#'
#' @param patientsPerSite a data.frame of per-community data.frames
#' @param commsPerSite a data frame mapping community ID to name
#'
#' @return patientsPerSite, but with the community name column appended
#' @export
Pt._DenormalizeCommunityNames <- function(patientsPerSite, commsPerSite) {
  pmap(
    list(x = patientsPerSite, y = commsPerSite),
    function(x, y) { dplyr::inner_join(x, y, by = c("Comunidad" = "ID")) }
  )
}

Pt._ParseAndFixBirthdates <- function(patients) {
  vprint("")
  vprint("ParseAndFixBirthdates started...")
  patients <- add_column(patients, birthdate = NA)
  patients <- add_column(patients, birthdate.is.estimated = NA)
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  for (i in 1:nrow(patients)) {
    if (VERBOSE) {
      svMisc::progress(i)
    }
    pt <- patients[i, ]
    estimated <- FALSE
    y <- pt[["FN_Ano"]]
    if (is.na(y) | y < 1900 | y > currentYear) {
      y <- 1900
      estimated <- TRUE
    }
    m <- pt[["FN_Mes"]]
    if (is.na(m) | m < 1 | m > 12) {
      m <- 1
      estimated <- TRUE
    }
    d <- pt[["FN_Dia"]]
    if (is.na(d) | d < 1 | d > 31) {
      d <- 1
      estimated <- TRUE
    }
    patients[i, "birthdate"] <- paste(y, m, d, sep = "-")
    patients[i, "birthdate.is.estimated"] <- estimated
  }
  return(patients)
}

Pt._ParseIdentifier <- function(oldId, location) {
  ifelse(!is.na(oldId) & oldId != "",
         paste("Old Identification Number", oldId, location, sep = ":"),
         "")
}

Pt._CesIdentifier <- function(index, location) {
  prefix <- stringr::str_to_upper(substr(location, 1, 3))
  idNum <- 1000000 + index
  id <- paste0(prefix, idNum)
  paste("Chiapas EMR ID", id, location, sep = ":")
}

Pt._ParseGender <- function(g) {
  ifelse(is.na(g), "U", ifelse(g == "1", "F", "M"))
}

Pt._ParseAddresses <- function(addr) {
  paste("cityVillage", addr, sep = ":")
}

Pt._CreateDistinctPatients <- function(patients,
                                       cesid,
                                       removeIfNoBirthYear = FALSE) {
  # split "selected" from patients
  selected <- patients %>% filter(patients$CesID == cesid)
  patients %<>% filter(patients$CesID != cesid)
  if (removeIfNoBirthYear) {
    # only preserve "selected" pts who have birth year
    selected %<>% filter(!is.na(selected$FN_Ano))
  }
  # append an index to the CesID of each selected pt
  for (i in seq_len(nrow(selected))) {
    selected[i, "CesID"] <- paste(selected[i, "CesID"], i, sep = "-")
  }
  # glue them back together
  return(rbind(patients, selected))
}

#' ManualDedupe
#'
#' @param patients (tbl) : the full patients table, flattened
#'
#' @return patients (tbl) with a few duped patients split up
#' @export
Pt._ManualDedupe <- function(patients) {
  
  badIds <- c(
    "001-000020",
    "002-000039",
    "077-000011",
    "001-000005", 
    "001-000031", 
    "001-000041", 
    "009-000006", 
    "009-000007", 
    "011-000010", 
    "011-000014", 
    "028-000001", 
    "035-000003", 
    "047-000010", 
    "100-000001", 
    "139-000001", 
    "142-000002",
    "001-000001",
    "001-000011",
    "001-000012",
    "001-000016",
    "001-000023",
    "001-000025",
    "001-000028",
    "001-000030",
    "001-000036",
    "001-000037",
    "002-000016",
    "002-000022",
    "002-000023",
    "002-000030",
    "002-000041",
    "007-000013",
    "011-000003",
    "011-000008",
    "011-000024",
    "011-000027",
    "012-000001",
    "012-000003",
    "016-000002",
    "017-000002",
    "017-000005",
    "025-000003",
    "025-000007",
    "025-000014",
    "025-000018",
    "025-000021",
    "025-000026",
    "025-000032",
    "025-000038",
    "025-000047",
    "025-000053",
    "025-000055",
    "025-000057",
    "030-000004",
    "030-000006",
    "051-000003",
    "055-000001",
    "059-000008",
    "063-000001",
    "064-000002",
    "066-000002",
    "067-000001",
    "070-000001",
    "072-000001",
    "077-000006",
    "077-000016",
    "077-000018",
    "077-000020",
    "077-000021",
    "077-000023",
    "079-000005",
    "084-000002",
    "086-000001",
    "087-000006",
    "087-000018",
    "087-000027",
    "087-000030",
    "087-000034",
    "087-000037",
    "087-000049",
    "087-000050",
    "088-000029",
    "088-000031",
    "092-000025",
    "092-000025",
    "092-000043",
    "092-000047",
    "092-000048",
    "100-000002")
  
  patients <- reduce(badIds,
                     Pt._CreateDistinctPatients,
                     removeIfNoBirthYear = TRUE,
                     .init = patients)
  return(patients)
}

#' Deduplicate
#'
#' @param patients (tbl) : the full patients table, flattened
#' 
#' Removes all the entries that have the same CesID and names, preserving as
#' much information in the other columns as possible. Freaks out if there are
#' discrepancies between columns.
#'
#' @return patients (tbl)
#' @export
#'
#' @examples
Pt._Deduplicate <- function(patients) {
  
  # do some manual fixes of a few edge cases
  patients %<>% Pt._ManualDedupe
  # create column of info on which to dedupe
  patients <- dplyr::bind_cols(patients,
      idAndNames = paste(patients$CesID, patients$Nombre, patients$Apellido))
  dupeGroups <- Util.GetDupeGroups(patients, "idAndNames")
  
  # CollapseDupeGroup: collapses groups into individual entries
  CollapseDupeGroup <- function(group) {
    intCols <- c("FN_Ano", "FN_Mes", "FN_Dia")
    res <- group[1, ]
    # take non-NA entries from int cols
    for (col in intCols) {
      goodItem <- unique(group[[col]][which(!is.na(group[[col]]))])
      if (length(goodItem) == 1) {
        res[[col]] <- goodItem
      }
      if (length(goodItem) > 1) {
        res[[col]] <- goodItem[[1]]
        print(paste("WARNING: multiple values for", col, "for person", res$idAndNames, ": ", paste(goodItem, collapse = " "),
            "    Patient exists in ", paste(group$commName, collapse = " "),
            "    Consider handling the patient in ManualDedupe(...)"))
      }
    }
    # prefer non-empty strings
    for (col in PT_COLS_CHAR) {
      goodItem <- unique(group[[col]][which(group[[col]] != "")])
      if (length(goodItem) == 1) {
        res[[col]] <- goodItem
      }
      if (length(goodItem) > 1) {
        res[[col]] <- goodItem[[1]]
        print(paste("WARNING: multiple values for", col, "for person", res$idAndNames, ": ", paste(goodItem, collapse = " "),
            "    Patient exists in ", paste(group$commName, collapse = " "),
            "    Consider handling the patient in ManualDedupe(...)"))
      }
    }
    # prefer false positives to false negatives
    for (col in PT_COLS_LOGICAL) {
      trueRows <- group[[col]][which(group[[col]])]
      if (length(trueRows) > 0) {
        res[[col]] <- TRUE
      }
    }
    return(res)
  }
  
  # collapse all duplicates into individual entries
  mergedPts <- map(dupeGroups, CollapseDupeGroup)
  # remove all duplicated entries from patients
  patients <- patients[!Util.AllDuplicated(patients$idAndNames), ]
  # add the merged patients back in
  patients <- bind_rows(patients, mergedPts)
}

Pt._SplitDuplicatedCesIds <- function(patients) {
  dupedIds <- as.vector(patients[Util.AllDuplicated(patients$CesID), ][["CesID"]])
  pb = txtProgressBar(min = 0, max = length(dupedIds), initial = 0, style = 3)
  for (i in seq_along(dupedIds)) {
    setTxtProgressBar(pb, i)
    patients <- Pt._CreateDistinctPatients(patients,
                                           dupedIds[i],
                                           removeIfNoBirthYear = FALSE)
  }
  close(pb)
  return(patients)
}

# Interface Functions ##########################################################

#' FetchData
#'
#' Pulls data from `/data/input/<community>/Pacientes.csv` and 
#' `Comunidades.csv`.
#' 
#' @return (tbl) with `$patientsPerSite` and `$communitiesPerSite`
Pt.FetchData <- function() {
  vprint("FetchData started")
  res <- tibble(
    patientsPerSite = map(paste0(Util.CommunityPaths(), "/Pacientes.csv"),
                          Util.CsvToTibble),
    communitiesPerSite = map(paste0(Util.CommunityPaths(), "/Comunidades.csv"),
                             Util.CsvToTibble)
  )
  vprint("FetchData done")
  return(res)
}

#' CleanTable
#' 
#' Filters invalid patient entries.
#' Fixes fields that can be fixed.
#' Parses dates, assembles the birthday.
#' 
#' @param input (tbl | df) has `$patientsPerSite` and `$communitiesPerSite`
#' 
#' @return (data.frame) all the patient data, cleaned up and flattened
Pt.CleanTable <- function(input) {
  vprint("CleanTable started")
  patients <- Pt._DenormalizeCommunityNames(
    input$patientsPerSite, input$communitiesPerSite
  )
  vprint("..AppendClinicNames")
  patients %<>% Util.AppendClinicNames(Util.CommunityPaths())
  vprint("..Flattening")
  patients <- do.call(rbind, patients)  # flatten
  vprint("..tibble")
  patients %<>% as_tibble()
  vprint("..Unfactorize")
  patients %<>% Pt._Unfactorize()
  vprint("..Deduplicate")
  patients %<>% Pt._Deduplicate()
  vprint("..SplitDuplicatedCesIds")
  patients <- Pt._SplitDuplicatedCesIds(patients)
  vprint("..ParseAndFixBirthdates")
  patients %<>% Pt._ParseAndFixBirthdates()
  vprint("..Parsing registration dates")
  patients %<>% dplyr::bind_cols(
      createdDate = Util.TransformDate(patients[["Fechar.de.registro"]]))
  vprint("..FixInvalidData")
  patients %<>% Pt._FixInvalidData()
  vprint("..FilterUnsalvagableData")
  patients %<>% Pt._FilterUnsalvagableData()
  vprint("..FixDatatypes")
  patients %<>% Pt._FixDatatypes()
  vprint("..Appending UUIDs")
  patients <- add_column(patients, ptUuid = Pt._GeneratePtUuid(patients))
  vprint("CleanTable done")
  return(patients)
}

#' GetCleanedTable
#' 
#' Pulls data from `/data/input/<community>/Pacientes.csv` and 
#' `Comunidades.csv`. Merges and cleans up the data.
#' Filters invalid patient entries. Deduplicates and splits patients.
#' 
#' @param useCache (lgl) defaults to fall back on the global flag
#' 
#' References global flag `USE_CLEAN_PT_DATA_CACHE`.
#' 
#' @return (data.frame) all the patient data, cleaned up and flattened
#' @export
Pt.GetCleanedTable <- function(useCache = NA) {
  # Use the cached file only if the argument flag `useCache` is TRUE, or if
  # the argument flag is not specified and the global flag is TRUE.
  UseCache <- function() {
    res <- useCache || (is.na(useCache) && USE_CLEAN_PT_DATA_CACHE)
    return(!is.na(res) && res)
  }
  if (UseCache()) {
    if (file.exists(CLEAN_PT_DATA_CACHE)) {
      print(paste("Using cached patient data at", CLEAN_PT_DATA_CACHE))
      return(read_csv(CLEAN_PT_DATA_CACHE))
    } else {
      print("Would use cache, but no cached patient data is present.")
    }
  }
  input <- Pt.FetchData()
  outputData <- Pt.CleanTable(input)
  if (UseCache() == TRUE) {
    print(paste("Saving patient data to cache at", CLEAN_PT_DATA_CACHE))
    write_csv(outputData, CLEAN_PT_DATA_CACHE, na = "")
  }
  return(outputData)
}

Pt.PrepareOutputData <- function(cleanPtData) {
  identifiers <- paste(
    Pt._ParseIdentifier(cleanPtData[["CesID"]], cleanPtData[["commName"]]),
    Pt._CesIdentifier(1:nrow(cleanPtData), cleanPtData[["commName"]]),
    sep = ";"
  )
  
  return(tibble(
    "uuid" = cleanPtData[["ptUuid"]],
    "Identifiers" = identifiers,
    "Given names" = cleanPtData[["Nombre"]],
    "Family names" = cleanPtData[["Apellido"]],
    "Gender" = Pt._ParseGender(cleanPtData[["Sexo"]]),
    "Birthdate" = cleanPtData[["birthdate"]],
    "Date created" = cleanPtData[["createdDate"]],
    "Addresses" = Pt._ParseAddresses(cleanPtData[["Comunidades"]]),
    "Void/Retire" = FALSE
  ))
}
