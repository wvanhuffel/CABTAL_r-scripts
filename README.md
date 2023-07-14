# CABTAL_r-scripts
R-scripts (v4.3.1) For the analysis of deidentified data from a survey regarding usage technical applications within a healthcare organisation. 
## Note: 
The data is not provided but the csv file structure utlised is:
  1.   id = col_double() the identifier of the person of submitting (to allow alias association should the originator need to be contacted
  2.   svc = col_character(), "Service" e.g. Peadiatrics, Emergency Medicine; Community Services;
  3.   rgn = col_character(), "Region" e.g. North, South, East, N, SSE...
  4.   ocp = col_character(), "Occupation" e.g. Nurse, Doctor, Adninistration...
  5.   `TAL-S-001` = col_double(),
  6.   `TAL-S-<NNN>` = col_double()

Seperate Alias (private, location specific) files for:
1. Id to PII (for responder resolution)
2. Technology Titles (Human Readable, e.g. TAL-S-001 = "Local PAS System A")
