library("tidyverse")

Util.AllDuplicated <- function(vec) {
  duplicated(vec) | duplicated(vec, fromLast = TRUE)
}

#' AnyEntryNotNa
#'
#' @param data (tbl)
#' @param colNames (c[str]) vector of column names
#'
#' @return c(lgl) along rows of data, whether or not any entry is not NA
Util.AnyEntryNotNa <- function(data, colNames) {
  entriesNotNa <- !is.na(data[colNames])
  return(apply(entriesNotNa, 1, any))
}

Util._CommNamesFromCommunityPaths <- function(communityPaths) {
  communityPaths   %>%
    strsplit("/", fixed = TRUE) %>%
    map(function(p) { p[3] })   %>%
    unlist()                    %>%
    {ifelse(. == "Laguna", "Laguna del Cofre", .)} %>%
    {ifelse(. %in% c("Plan_Alta", "Plan_Baja"), "Plan de la Libertad", .)}
}

Util.FlattenWithClinicNamesTypeSafe <- function(perCommunityTbl, communityPaths) {
  commNames <- Util._CommNamesFromCommunityPaths(communityPaths)
  tblsWithComms <- tibble(commName = commNames, data = perCommunityTbl)
  fullTbl <- unnest(tblsWithComms)
  return(fullTbl)
}

Util.AppendClinicNames <- function(perCommunityTbl, communityPaths) {
  commNames <- Util._CommNamesFromCommunityPaths(communityPaths)
  tableWithClinics <- map2(
    perCommunityTbl,
    commNames,
    function(df, commName) {
      add_column(df, commName = commName)
    }
  )
  return(tableWithClinics)
}

Util.CommunityPaths <- function() {
  list.dirs("data/input")[-1] # element 1 is "data/input" itself
}

Util.ConvertTextToBoolVector <- function(strVector) {
  ConvertTextToBool <- function(valStr) { eval(parse(text = valStr)) }
  return(unlist(map(toupper(strVector), ConvertTextToBool)))
}

Util.CsvToTibble <- function(path) {
  as_tibble(read.csv(path))
}

#' GetDupeGroups
#'
#' @param table (tbl)
#' @param key (str) the name of the column on which to identify duplicates
#'   i.e., all rows of `table` with the same value for `table[[key]]` will
#'   be grouped together
#'
#' @return list[table] where each list item is a table indexed by a unique
#'   `table[[key]]`, which is shared between all the rows in that table
Util.GetDupeGroups <- function(table, key) {
  dupes <- table[Util.AllDuplicated(table[[key]]), ]
  dupeGroups <- split(dupes, dupes[[key]])
  return(dupeGroups)
}

Util.ParseAccessDate <- function(dateString) {
  lubridate::parse_date_time(
    dateString, "%a %b %d %H:%M:%S ... %Y"
  )
}

Util.CoerceDateIfInvalid <- function(dateObj) {
  DateIsInvalid <- function(date) {
    is.na(date) | lubridate::year(date) < 1990 | difftime(Sys.Date(), date) < 0
  }
  defaultDate <- as.POSIXct(lubridate::make_date(1990, 1, 1))
  dateObj <- if_else(DateIsInvalid(dateObj), defaultDate, dateObj)
  return(dateObj)
}

Util.IsoDateString <- function(dateObj) {
  format.Date(dateObj, "%Y-%m-%dT%H:%M:%S%z")
}

#' TransformDate
#' 
#' Parses an Access-formatted date, validates it, and returns the ISO date.
#' Assumes that all input dates are CST.
#' 
#' Dates before 1980 or in the future are coerced to 1990-01-01. This coersion
#' is appropriate for metadata like "consult date," for which 1990 cannot be
#' confused for a legitimate date. It is inappropriate for birthdays, which
#' could perfectly reasonably be before 1980, or equal to 1990-01-01.
#'
#' @param dateString (str) like 'Mon Jan 12 00:00:00 CST 2015'
#'
#' @return ISO datestring like '2015-01-12T00:00:00Z'
Util.TransformDate <- function(dateString) {
  dateString %>%
    Util.ParseAccessDate() %>%
    Util.CoerceDateIfInvalid() %>%
    Util.IsoDateString()
}

#' UuidHash
#'
#' @param A string to hash with SHA1
#'
#' @return A UUID string with dashes
Util.UuidHash <- function(str) {
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

Util.YesNoConceptForBoolean <- function(value) {
  ifelse(value, "PIH:YES", "PIH:NO")
}
