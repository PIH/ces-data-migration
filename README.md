# CES Data Migration

R scripts for migrating Compañeros en Salud clinical data from
MS Access to OpenMRS.

Takes CSVs as input. Use ces-access-export to dump CSVs from a
MS Access database. Drop them into `input/`.

Produces CSVs readable by
[Initializer module](https://github.com/mekomsolutions/openmrs-module-initializer/).

