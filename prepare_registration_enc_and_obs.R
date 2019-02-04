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

