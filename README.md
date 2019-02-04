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

