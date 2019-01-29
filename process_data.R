ParseGender <- function(g) {
  return "U" if (g == NA) else ("F" if (g == "1") else "M")
}

ParseBirthdate <- function(y, m, d) {
  if (is.na(m) || is.na(d)) {
    if (is.na(y)) {
      return NA
    } else {
      return paste("01-01", y, sep="-")
    # }
  }
}

communityPaths = list.dirs("data/input")[-1]  # element 1 is "data/input" itself

input.patients <- read.csv(paste(communityPaths[1], "/Pacientes.csv", sep=""))

output.patients = data.frame(
  "Given names"=input.patients["Nombre"],
  "Familiy names"=input.patients["Apellido"],
  "Gender"=ParseGender(input.patients["Sexo"])
  "Birthdate"=ParseBirthdate(input.patients["FN_Ano"], input.patients["FN_Mes"], input.patients["FN_Dia"])
  "Addresses"=ParseAddresses(input.patients["Comunidad"])
  )
