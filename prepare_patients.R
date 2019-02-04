library(purrr)

UuidHash <- function(str) {
  hash <- map(str, digest::sha1)
  return(
    paste(
      substr(hash, 1, 8),
      substr(hash, 9, 12),
      substr(hash, 13, 16),
      substr(hash, 17, 20),
      substr(hash, 21, 32),
      sep = "-"
    )
  )
}

GeneratePtUuid <- function(table) {
  UuidHash(
    paste(
      unlist(table["Nombre"]),
      unlist(table["Apellido"]),
      unlist(table["Fechar.de.registro"])
    )
  )
}

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

ParseBirthdate <- function(y, m, d) {
  ifelse((is.na(m) || is.na(d)),
    ifelse(is.na(y),
      NA,
      paste(y, "01-01", sep = "-")
    ),
    paste(y, m, d, sep = "-")
  )
}

ParseAddresses <- function(addr) {
  paste("cityVillage", addr, sep = ":")
}

ParseDateCreated <- function(dateString) {
  dates <- lubridate::parse_date_time(
    dateString, "%a %b %d %H:%M:%S ... %Y",
    tz = "CDT"
  )
  return(format.Date(dates, "%Y-%m-%dT%H:%M:%SZ"))
}

DenormalizeCommunityNames <- function(patientsPerSite, commsPerSite) {
  pmap(
    list(
      x = patientsPerSite, y = commsPerSite,
      by.x = "Comunidad", by.y = "ID"
    ),
    merge
  )
}

AppendClinicNames <- function(patientsPerSite, communityPaths) {
  commNames <- communityPaths   %>%
    strsplit("/", fixed = TRUE) %>%
    map(function(p) { p[3] })   %>%
    unlist()                    %>%
    {ifelse(. == "Laguna", "Laguna del Cofre", .)} %>%
    {ifelse(. %in% c("Plan_Alta", "Plan_Baja"), "Plan de la Libertad", .)}
  setNames(patientsPerSite, commNames)
  patientsWithClinics <- map2(
    patientsPerSite,
    commNames,
    function(df, commName) {
      cbind(df, commName)
    }
  )
  return(patientsWithClinics)
}

# Import data
communityPaths <- list.dirs("data/input")[-1] # element 1 is "data/input" itself
input.patientsPerSite <- map(paste0(communityPaths, "/Pacientes.csv"), read.csv)
input.communitiesPerSite <- map(paste0(communityPaths, "/Comunidades.csv"), read.csv)

# Prep and filter patients table
tmp.patientsWithComms <- DenormalizeCommunityNames(
  input.patientsPerSite, input.communitiesPerSite
)
tmp.patientsWithClinics <- AppendClinicNames(tmp.patientsWithComms, communityPaths)
tmp.patientsUnfiltered <- do.call(rbind, tmp.patientsWithClinics)
tmp.patients <- tmp.patientsUnfiltered[
  !(tmp.patientsUnfiltered$Nombre == "" & tmp.patientsUnfiltered$Apellido == ""),
]

# Extract attributes
birthdateList <- list(
  tmp.patients["FN_Ano"][, ],
  tmp.patients["FN_Mes"][, ],
  tmp.patients["FN_Dia"][, ]
)
birthdates <- unlist(pmap(birthdateList, ParseBirthdate))
createdDates <- ParseDateCreated(unlist(tmp.patients["Fechar.de.registro"]))
identifiers <- paste(
  ParseIdentifier(unlist(tmp.patients["CesID"]), unlist(tmp.patients["commName"])),
  CesIdentifier(1:dim(tmp.patients)[1], unlist(tmp.patients["commName"])),
  sep = ";"
)

output.patients <- data.frame(
  "uuid" = GeneratePtUuid(tmp.patients),
  "Identifiers" = identifiers,
  "Given names" = unname(tmp.patients["Nombre"]),
  "Family names" = unname(tmp.patients["Apellido"]),
  "Gender" = ParseGender(tmp.patients["Sexo"][, ]),
  "Birthdate" = birthdates,
  "Date created" = createdDates,
  "Addresses" = ParseAddresses(tmp.patients["Comunidades"][, ]),
  "Void/Retire" = FALSE,
  check.names = FALSE # allow column names to have spaces
)

write.csv(output.patients, "data/output/patients.csv",
  row.names = FALSE, na = ""
)
