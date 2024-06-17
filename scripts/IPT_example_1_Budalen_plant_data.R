##----------------------------------------------------------------------------##
# IPT_example_1_Budalen_plant_data
# This script contains code to make a Budalen Plant Survey Darwin Core Compliant
##----------------------------------------------------------------------------##

# 0.PACKAGES ----
library(here)
library(data.table)
library(tidyverse)
library(uuid)
library(rgbif)

# 1. READ IN DATA ----

# Read in the vegetation data we are working on
veg_data <- read.csv(here("raw_data", "Clean_Data_FloweringPlant.csv"),
                     sep = ";")

# Read in example dataset
example_data <- fread(here("raw_data", "example_veg_data",
                           "occurrence.txt"))

# Read in example event data
example_event_grazing <- fread(here("raw_data", "example_veg_data",
                            "event.txt"))

example_event_setesdal <- fread(here("raw_data", "example_setesdal_data",
                                    "event.txt"))

# 2. CREATE EVENT FILE ----

# Check columns in example event
colnames(example_event_grazing)

## 2.1. Subset vegetation data to contain the needed columns ----
veg_event_truncated <-  veg_data |>
  select(2, 4:6) |>
  mutate(eventID_temporary = str_c(Site, Transect, Quadrat, sep = "_"),
         parentEventID_temporary = str_c(Site, Transect, sep = "_"),
         eventRemarks = "quadrat sampling",
         id = sapply(1:n(), function(x) UUIDgenerate()),
         type = "Event",
         ownerInstitutionCode = "NTNU-VM",
         eventID = id,
         year = "2023",
         continent = "Europe",
         country = "Norway",
         municipality = "Budal",
         decimalLatitude = NA_real_,
         decimalLongitude = NA_real_,
         geodeticDatum = "WGS84",
         month = as.integer(format(as.Date(Date, format = "%d.%m.%Y"), "%m")),
         day = as.integer(format(as.Date(Date, format = "%d.%m.%Y"), "%d")),
         parentEventID = NA_real_) |>
  select(eventID_temporary, parentEventID_temporary, eventRemarks, 
         eventID, parentEventID, Date, Site, Transect, Quadrat,
         id, type, ownerInstitutionCode, year, continent, country,
         municipality, decimalLatitude, decimalLongitude, geodeticDatum,
         month, day)

## 2.2. Add the unique transects as events and give them IDs ----

# Extract the unique transect names 
unique_transectIDs <- unique(veg_event_truncated$parentEventID_temporary)

# Map parentEventID_temporary to Site
parent_to_site <- veg_event_truncated |>
  select(parentEventID_temporary, Site) |>
  distinct() |>
  drop_na()

# Create new dataframe for the unique rows
new_rows_transects <- data.frame(
  eventID_temporary = unique_transectIDs,
  parentEventID_temporary = parent_to_site$Site[match(unique_transectIDs, parent_to_site$parentEventID_temporary)],
  eventRemarks = "transect event",
  eventID = sapply(1:length(unique_transectIDs), function(x) UUIDgenerate()),
  parentEventID = NA,
  Date = NA,
  Site = NA,
  Transect = NA,
  Quadrat = NA,
  id = NA,
  type = "Event",
  ownerInstitutionCode = "NTNU-VM",
  year = "2023",
  continent = "Europe",
  country = "Norway",
  municipality = "Budal",
  decimalLatitude = NA,
  decimalLongitude = NA,
  geodeticDatum = "WGS84",
  month = NA,
  day = NA,
  stringsAsFactors = FALSE
)

# Give values to sites column
new_rows_transects <- new_rows_transects |>
  mutate(Site = parentEventID_temporary,
         Transect = rep(1:5, 9))


# Bind rows at the top of the original df
veg_event_transects <- rbind(new_rows_transects, veg_event_truncated)

## 2.3. Add the unique sites as events and give them IDs ----

# Extract unique site names
unique_siteIDs <- unique(veg_event_truncated$Site)

# Create new dataframe for the unique Site rows
new_rows_sites <- data.frame(
  eventID_temporary = unique_siteIDs,
  parentEventID_temporary = NA,
  eventRemarks = "Site",
  eventID = sapply(1:length(unique_siteIDs), function(x) UUIDgenerate()),
  parentEventID = NA,
  Date = NA,
  Site = NA,
  Transect = NA,
  Quadrat = NA,
  id = NA,
  type = "Event",
  ownerInstitutionCode = "NTNU-VM",
  year = "2023",
  continent = "Europe",
  country = "Norway",
  municipality = "Budal",
  decimalLatitude = NA,
  decimalLongitude = NA,
  geodeticDatum = "WGS84",
  month = NA,
  day = NA,
  stringsAsFactors = FALSE
)

# Add values to sites column
new_rows_sites <- new_rows_sites |>
  mutate(Site = eventID_temporary)

# Bind rows at the top of the original df
veg_event_sites <- rbind(new_rows_sites, veg_event_transects)

## 2.4. Map parentEventIDs to the eventIDs ----

# Create mapping from event_ID_temporary to event_ID
id_mapping <- veg_event_sites |>
  select(eventID_temporary, eventID) |>
  distinct() |>
  drop_na()

# Create function that will apply this mapping 
find_parent_eventID <- function(parent_event_temp) {
  if (is.na(parent_event_temp)) {
    return(NA)  # return NA for rows where parentEventID_temporary is NA
  } else {
    # find the eventID corresponding to the parentEventID_temporary
    event_id <- id_mapping$eventID[id_mapping$eventID_temporary == parent_event_temp]
    if (length(event_id) > 0) {
      return(event_id)
    } else {
      return(NA)  # return NA if no corresponding eventID is found
    }
  }
}


# Apply function to dataframe
veg_event <- veg_event_sites |>
  mutate(parentEventID = sapply(parentEventID_temporary, find_parent_eventID))

# Remove unneccesary columns are re-order columns
veg_event <- veg_event |>
  # rename some columns
  rename(site = Site,
         locationID = Transect,
         fieldNumber = Quadrat) |>
  # add elevation column - empty for noe
  mutate(verbatimElevation = NA,
         id = eventID) |>
  # reorder columns
  select(id, type, ownerInstitutionCode, eventID, parentEventID, year,
         month, day, site, locationID, fieldNumber, continent, country,
         municipality, verbatimElevation, decimalLatitude, decimalLongitude) |>
  # add elevation, latitude and longitude - data just received
  mutate(verbatimElevation = case_when(
    site == "FH1" ~ 755,
    site == "FH2" ~ 789,
    site == "FH3" ~ 819,
    site == "EG1" ~ 843,
    site == "EG2" ~ 871,
    site == "EG3" ~ 858,
    site == "IG1" ~ 778,
    site == "IG2" ~ 781,
    site == "IG3" ~ 782),
    decimalLatitude = case_when(
      site == "FH1" ~ 62.734964,
      site == "FH2" ~ 62.734905,
      site == "FH3" ~ 62.736355,
      site == "EG1" ~ 62.732242,
      site == "EG2" ~ 62.731253,
      site == "EG3" ~ 62.732208,
      site == "IG1" ~ 62.728650,
      site == "IG2" ~ 62.728993,
      site == "IG3" ~ 62.729156),
    decimalLongitude = case_when(
      site == "FH1" ~ 10.683115,
      site == "FH2" ~ 10.686017,
      site == "FH3" ~ 10.683845,
      site == "EG1" ~ 10.700742,
      site == "EG2" ~ 10.705989,
      site == "EG3" ~ 10.702540,
      site == "IG1" ~ 10.702922,
      site == "IG2" ~ 10.702122,
      site == "IG3" ~ 10.701814
    )) 

# Write event file to folder
write_delim(veg_event, here("data", "event.txt"), delim = "\t")

## 2.5. Update event core with basisOfRecord ----

# Read in data
event <- fread(here("data", "event.txt"))

# Add basisOfRecord column
event <- event |>
  mutate(basisOfRecord = "HumanObservation")

# Write event file to folder
write_delim(event, here("data", "event.txt"), delim = "\t")

# 3. CREATE OCCURRENCE FILE ----

## 3.1. Prepare data for occurrence file ----

# Remove unnecessary columns
veg_truncated <- veg_data |>
  select(4:6, 8:53) |>
  # create new column that combines site_transect_quadrat
  mutate(quadrat_id = str_c(Site, Transect, Quadrat, sep = "_"))

# Convert to long format
veg_long <- veg_truncated |>
  select(-c(Site, Transect, Quadrat)) |>
  # add columns: id, institutionCode, ownerInstitutionCode, basisOfRecord, occurrenceID, organismQuantityType
  pivot_longer(cols = 1:46,
               names_to = "scientificName",
               values_to = "organismQuantity") |>
  mutate(id = NA_real_,
         institutionCode = "NTNU-VM",
         ownerInstitutionCode = "NTNU-VM",
         basisOfRecord = "HumanObservation",
         occurrenceID = sapply(1:n(), function(x) UUIDgenerate()),
         organismQuantityType = "Frequency",
         kingdom = "Plantae")

## 3.2. Add eventIDs of quadrats to the id column for each record ----

# Make a 2-column df to map eventID to the quadratID
veg_event_truncated_ids <- veg_event_truncated |>
  select(eventID_temporary, eventID)

# Create mapping
map <- veg_event_truncated_ids |> 
  deframe()

# Add id values to the vegetation dataframe
veg_long_id <- veg_long |>
  mutate(id = map[quadrat_id])

## 3.3. Final adjustments to occurrence file ----

# Remove the underline from species names
veg_spp_name <- veg_long_id |>
  mutate(scientificName = gsub("_", " ", scientificName),
         scientificName = str_replace_all(scientificName, " sp", "")) |>
  # reorder columns
  select(id, institutionCode, ownerInstitutionCode, basisOfRecord, occurrenceID,
         organismQuantity, organismQuantityType, scientificName,
         kingdom)

# Get a list of the species only identified to genus level
genus_only <- veg_spp_name |>
  filter(str_count(scientificName, pattern = "\\s") == 0)

# Get species and genus names
sorted_unique_species <- sort(unique(veg_spp_name$scientificName))
unique(genus_only$scientificName) #"Alchemilla" "Taraxacum"  "Hieracium"  "Myosotis"

# Add column in df stating the smallest identified rank
veg_spp_rank <- veg_spp_name |>
  mutate(scientificNameRank = if_else(scientificName %in% c("Alchemilla","Taraxacum",
                                                            "Hieracium" ,"Myosotis"), "genus", "species"))

## 3.4. Check species names ----

# Extract dataframe of backbone check 
checked_names <- as.data.frame(name_backbone_checklist(veg_spp_rank))

# Extract records where the matchType is not exact
flagged_records <- checked_names |>
  filter(matchType != "EXACT")

# Fix names of misspelled records
corrected_veg <- veg_spp_rank |>
  mutate(scientificName = str_replace_all(scientificName, "Ranuncuus acris", "Ranunculus acris"),
         scientificName = str_replace_all(scientificName, "Veronica serphyllifolia", "Veronica serpyllifolia"),
         # remove dot in Vaccinium vitis.idaea
         scientificName = str_replace_all(scientificName, "Vaccinium vitis.idaea", "Vaccinium vitis idaea"))

# Check species names again
rechecked_names <- as.data.frame(name_backbone_checklist(corrected_veg))

# Extract records where the matchType is not exact
reflagged_records <- rechecked_names |>
  filter(matchType != "EXACT")

# Save new data as an occurrence df
write_delim(corrected_veg, here("data", "occurrence.txt"), delim = "\t")


# END OF SCRIPT ----