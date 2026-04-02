# Data Dictionary

| variable | type  | na   | %na | unique | description |
| -------- | ----  | ---: | -----: | -----: | ----------- |
 | DEPARTMENT | chr | 0 | 0 | 25 |First-level political/administrative division in Paraguay used to locate the record.| 
 | DISTRICT | chr | 0 | 0 | 258 | Administrative subdivision within a department; provides finer territorial precision.| 
 | LOCALITY | chr | 0 | 0 | 1366 |Specific population center (city, town, or community) where the record was collected.| 
 | YEAR | int | 1776 | 8.8 | 26 | Calendar year corresponding to the entomological record or event.| 
 | DATE | oth | 5749 | 28.3 | 763 | Exact date of the record (YYYY-MM-DD format).| 
 | DISEASE | chr | 0 | 0 | 6 |Name of the vector-borne disease associated with the epidemiological event.| 
 | SPECIES_RAW | chr | 0 | 0 | 128 |Original, uncleaned vector organism name as recorded in the raw surveillance data.| 
 | UTM_ZONE | int | 0 | 0 | 1 | UTM zone of the coordinate reference system for the geographic point.| 
 | UTM_HEMISPHERE | chr | 0 | 0 | 1 |Indicates whether the UTM coordinate is in the Northern (N) or Southern (S) hemisphere.| 
 | UTM_X | chr | 1 | 0 | 4792 | Easting coordinate in meters within the UTM system.| 
 | UTM_Y | chr | 0 | 0 | 4796 | Northing coordinate in meters within the UTM system.| 
 | LAT | dbl | 0 | 0 | 5328 | Geographic latitude in decimal degrees (WGS84).| 
 | LONG | dbl | 0 | 0 | 5332 |Geographic longitude in decimal degrees (WGS84).| 
 | GEOREFERENCING_TYPE | chr | 0 | 0 | 49 | Method or source used to assign the location (e.g., GPS, digital cartography, administrative centroid).| 
 | DATA_SOURCE | chr | 0 | 0 | 6 |Provenance tracking field indicating the original raw file from which the record was extracted.| 
 | SPECIES_CLEAN | chr | 0 | 0 | 115 |Vector name after initial string cleaning, normalization, and removal of special characters.| 
 | SPECIES_MATCHED | chr | 0 | 0 | 74 | Vector name successfully matched against a standardized taxonomic database.| 
 | STATUS | chr | 0 | 0 | 3 | Taxonomic matching status (e.g., accepted, synonym, ambiguous).| 
 | CONFIDENCE | int | 5 | 0 | 11 |Estimated confidence level of the morphological identification or taxonomic matching.| 
 | KINGDOM | chr | 0 | 0 | 2 |Taxonomic kingdom of the identified specimen.| 
 | PHYLUM | chr | 0 | 0 | 2 | Taxonomic phylum of the identified specimen.| 
 | ORDER | chr | 0 | 0 | 3 |Taxonomic order of the identified specimen.| 
 | FAMILY | chr | 0 | 0 | 4 | Taxonomic family of the identified specimen (e.g., Culicidae, Reduviidae).| 
 | GENUS | chr | 0 | 0 | 22 | Taxonomic genus of the identified specimen.| 
 | SPECIES | chr | 0 | 0 | 58 | Standardized specific epithet of the identified specimen.| 
 | SPECIES_AI_Correct | chr | 0 | 0 | 94 |Final scientific name of the species after automated curation and correction by the AI agent.| 
 | Justification | chr | 0 | 0 | 113 |Documented rationale provided by the AI model explaining the specific taxonomic correction applied.| 
 | Gemini_AI.app | chr | 0 | 0 | 2 |Indicates the specific AI model version or application used to perform the taxonomic validation.| 

