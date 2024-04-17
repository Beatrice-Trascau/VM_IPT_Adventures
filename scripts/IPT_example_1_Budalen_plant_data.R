##----------------------------------------------------------------------------##
# IPT_example_1_Budalen_plant_data
# This script contains code to make a Budalen Plant Survey Darwin Core Compliant
##----------------------------------------------------------------------------##

# 0.PACKAGES ----
library(here)
library(data.table)
library(tidyverse)
library(uuid)
library(LivingNorwayR)

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

# 2. CREATE OCCURRENCE FILE ----

# Remove unneccesary columns
veg_truncated <- veg_data |>
  select(8:53)

# Convert to long format
veg_long <- veg_truncated |>
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

# Remove the underline from species names
veg_spp_name <- veg_long |>
  mutate(scientificName = gsub("_", " ", scientificName)) |>
  # reorder columns
  select(id, institutionCode, ownerInstitutionCode, basisOfRecord, occurrenceID,
         organismQuantity, organismQuantityType, scientificName,
         kingdom)

# Save new data as an occurrence df
write_delim(veg_spp_name, here("data", "occurrence.txt"), delim = "\t")

# 3. CREATE EVENT FILE ----

# Check columns in example event
colnames(example_event_grazing)

# Subset columns for Site, Transect and Quadrat from dataframe
veg_event_truncated <-  veg_data |>
  select(2, 4:6) |>
  mutate(eventID_temporary = str_c(Site, Transect, Quadrat, sep = "_"),
         parentEventID_temporary = str_c(Site, Transect, sep = "_"),
         eventRemarks = "quadrat sampling") |>
  select(eventID_temporary, parentEventID_temporary, eventRemarks, 
         Site, Transect, Quadrat)

  # add missing columns
  # mutate(id = sapply(1:n(), function(x) UUIDgenerate()),
  #        type = "Event",
  #        ownerInstitutionCode = "NTNU-VM",
  #        eventID = id,
  #        year = "2023",
  #        continent = "Europe",
  #        country = "Norway",
  #        municipality = "Budal",
  #        decimalLatitude = NA_real_,
  #        decimalLongitude = NA_real_,
  #        geodeticDatum = "WGS84")

# Re-arrange columns so the order matches
veg_event_reordered <- veg_event_truncated |>
  # extract month from Date column - then remove the others
  mutate(month = as.integer(format(as.Date(Date, format = "%d.%m.%Y"), "%m")),
         day = as.integer(format(as.Date(Date, format = "%d.%m.%Y"), "%d"))) |>
  # reorder columns
  select(id, type, ownerInstitutionCode, eventID, Site, year, month, day, Transect,
         Quadrat, continent, country, municipality, decimalLatitude, decimalLongitude,
         geodeticDatum) |>
  rename(parentEventID = Site,
         locationID = Transect,
         fieldNumber = Quadrat)


### Experimental zone -----
# Create Site event rows (with parentIDs)
sites <- veg_event_truncated |>
  distinct(Site) |>
  mutate(siteID = map_chr(row_number(), ~UUIDgenerate())) |>
  rename(site_name = Site)

# Create helper function to find matching sideID
find_siteID <- function(transect_name) {
  # Create a logical vector to test if each site_name is in the transect_name
  matches <- sapply(sites$site_name, function(name) str_detect(transect_name, pattern = paste0("^", name, "|", name, "_")))
  if (any(matches)) {
    # Return the siteID where there's a match
    return(sites$siteID[matches])
  } else {
    # Return NA if no matches
    return(NA)
  }
}

# Create Transect event rows (with parentIDs)
transects <- veg_event_truncated %>%
  distinct(parentEventID_temporary) %>%
  mutate(
    transectID = map_chr(row_number(), ~ UUIDgenerate()),
    parentID = sapply(parentEventID_temporary, find_siteID)
  )



