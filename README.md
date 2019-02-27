# CES Data Migration

R scripts for migrating Compañeros en Salud clinical data from
MS Access to OpenMRS.

Takes CSVs as input. Use ces-access-export to dump CSVs from a
MS Access database. Drop them into `input/`.

Produces CSVs readable by
[Initializer module](https://github.com/mekomsolutions/openmrs-module-initializer/).

## Developing

Use Posix newlines and UTF-8 encoding. Strip trailing horizontal
whitespace when saving.
In RStudio, check these settings under Options -> Code -> Saving.

[Packrat](https://rstudio.github.io/packrat/walkthrough.html) is used
for dependency management.

Use Roxygen-style docstrings. Use `./extract_markdown.sh` to append the file
docstrings to this README.


## Script Documentation

### export_complete_access_csv.R
Exports a giant CSV with all pts and consults

Imports CSVs from `data/input/<site>/`. Uses that data to produce
a giant CSV file. One line per consult, patient data is duplicated
over all their consult rows.


- `OUTPUT_PATH = "data/output/complete_access_data.csv"`

### prepare_consult_enc_and_obs.R
Prepares consult data for OpenMRS-Iniz

Imports CSVs from `data/input/<site>/Consultas.csv`. Uses that data to produce
two CSV files for each encounter type.

- `data/output/encounters-*.csv` contains the consult encounters, one line per patient.
- `data/output/obs-*.csv` contains all the observations for each of those consults.

These output CSVs should then be imported by OpenMRS Initializer module.

### prepare_patients.R
Converts patient data from Access format to OpenMRS-Iniz

Imports CSVs from `data/input/<site>/Pacientes.csv`. Uses that data to produce
a CSV file, `data/output/patients.csv`, which should be imported by OpenMRS
Initializer module.

Deduplicates patients with identical CesID, Nombre, and Apellido. If those
patients have different data, issues a warning. You should use that warning
to add that patient to `R/patient_util.R:ManualDedupe`, which splits these
patient entries instead of merging them. The duplicates handled by
`ManualDedupe` get `-1`, `-2`, etc. appended to their CesID.

Splits patients with identical CesID but different name or birthdate. Each
distinct patient is assigned a new CesID with suffix `-1`, `-2`, etc.


- `PT_OUTPUT_PATH = "data/output/patients.csv"`

### prepare_registration_enc_and_obs.R
Prepares registration data for OpenMRS-Iniz

Imports CSVs from `data/input/<site>/Pacientes.csv`. Uses that data to produce
two CSV files. `ENC_OUTPUT` contains the registration encounters, one line per
patient. `OBS_OUTPUT` contains all the observations, of which there will be 3-5
per patient.

These output CSVs should then be imported by OpenMRS Initializer module.


- `ENC_OUTPUT_PATH = "data/output/encounters_registration.csv"`
- `OBS_OUTPUT_PATH = "data/output/obs_registration.csv"`

