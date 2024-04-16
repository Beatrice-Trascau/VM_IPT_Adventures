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







