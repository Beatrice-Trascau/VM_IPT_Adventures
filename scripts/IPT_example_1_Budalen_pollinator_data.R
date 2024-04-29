##----------------------------------------------------------------------------##
# IPT_example_1_Budalen_plant_data
# This script contains code to make a Budalen Pollinator Survey Darwin Core Compliant
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(data.table)
library(tidyverse)
library(uuid)
library(rgbif)

# 1. READ IN DATA ----

# Read in pollinator data
raw_poll_data <- read.csv(here("raw_data", "Clean_Data_Interaction.csv"),
                          sep = ";")

# Read in event core created in IPT_example_1_Budalen_plant_data.R
event_core <- fread(here("data", "event.txt"))

# 2. CREATE POLLINATOR OCCURRENCE FILE ----

## 2.1. Prepare data for occurrence file ----

# Remove unnecessary columns
poll_data <- raw_poll_data |>
  select(4, 6:28) |>
  # create  new column that combines site_trasnsect
  rename(resourceScientificName = Fancy.name)

# Convert to long format 
poll_data_long <- poll_data |>
  select(-Site) |>
  # add columns: id, institutionCode, ownerInstitutionCode, basisOfRecord, occurrenceID, organismQuantityType
  pivot_longer(cols = 2:23,
               names_to = "scientificName",
               values_to = "organismQuantity") |>
  mutate(id = NA_real_,
         institutionCode = "NTNU-VM",
         ownerInstitutionCode = "NTNU-VM",
         basisOfRecord = "HumanObservation",
         occurrenceID = sapply(1:n(), function(x) UUIDgenerate()),
         resourceID = occurrenceID,
         resourceRelationshipID = sapply(1:n(), function(x) UUIDgenerate()),
         relationshipRemarks = "pollinator on plant",
         organismQuantityType = "Count",
         kingdom = "Animalia",
         resourceScientificName = gsub("_", " ", resourceScientificName))

# Add relatedResourceID based on the eventID of the site/transect in the event core









