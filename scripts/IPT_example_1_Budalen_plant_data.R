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
  # add missing columns
  mutate(id = sapply(1:n(), function(x) UUIDgenerate()),
         type = "Event",
         ownerInstitutionCode = "NTNU-VM",
         eventID = id,
         year = "2023",
         continent = "Europe",
         country = "Norway",
         municipality = "Budal",
         decimalLatitude = NA_real_,
         decimalLongitude = NA_real_,
         geodeticDatum = "WGS84")

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




