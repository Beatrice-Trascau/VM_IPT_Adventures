##----------------------------------------------------------------------------##
# IPT_example_1_Budalen_plant_data
# This script contains code to make a Budalen Pollinator Survey Darwin Core Compliant
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(data.table)
library(tidyverse)
library(uuid)
library(stringr)
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
  rename(resourceScientificName = Fancy.name,
         site = Site)

# Convert to long format 
poll_data_long <- poll_data |>
  #select(-Site) |>
  # add columns: id, institutionCode, ownerInstitutionCode, basisOfRecord, occurrenceID, organismQuantityType
  pivot_longer(cols = 3:24,
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

## 2.2. Add relatedResourceID ----

# Get eventID for sites from event core
site_IDs <- event_core |>
  select(c(site, eventID)) |>
  slice_head(n = 9) 

# Create mapping
map <- site_IDs |> 
  deframe()

# Add eventID to the pollination dataframe
poll_data_long_id <- poll_data_long |>
  mutate(relatedResourceID = map[site])

## 2.3. Add correct taxonomic values ----

# Check which species/groups are recorded
unique(poll_data_long_id$scientificName) #21

# Add phylum, class, order, suborder, family, genus
poll_data_taxonomy <- poll_data_long_id |>
  mutate(phylum = "Arthropoda",
         class = if_else(scientificName == "Araneae_sp", "Arachnida", "Insecta"),
         order = case_when(scientificName %in% c("Brachycera", "Nematocera_sp", "Syrphidae_sp") ~ "Diptera",
                           scientificName %in% c("Lepidoptera_sp.moth.", "Lepidoptera_sp", "Plebejus_idas",
                                                 "Bolonia_thore", "Bolonia_euphrosyne", "Bolonia_selene",
                                                 "Erebia_ligea") ~ "Lepidoptera",
                           scientificName == "Araneae_sp" ~ "Araneae",
                           scientificName %in% c("Bombus_sp", "Bombus_pratorum", "Bombus_hortorum.jonellus",
                                                 "Bombus_monticola.lapponicus", "Bombus_balteatus", "Apis_mellifera",
                                                 "Bombus_lapidarius", "Bombus_lucorum.terrestris", "Bombus_pascuorum",
                                                 "Bombus_consobrinus") ~ "Hymenoptera"),
         family = case_when(scientificName == "Syrphidae_sp" ~ "Syrphidae",
                            scientificName == "Plebejus_idas" ~ "Lycaenidae",
                            scientificName %in% c("Bolonia_thore", "Bolonia_euphrosyne", 
                                                  "Bolonia_selene") ~ "Nymphalidae",
                            scientificName %in% c("Bombus_sp", "Bombus_pratorum", "Bombus_hortorum.jonellus",
                                                  "Bombus_monticola.lapponicus", "Bombus_balteatus", "Apis_mellifera",
                                                  "Bombus_lapidarius", "Bombus_lucorum.terrestris", "Bombus_pascuorum",
                                                  "Bombus_consobrinus") ~ "Apidae"),
         genus = case_when(scientificName == "Plebejus_idas" ~ "Plebejus",
                           scientificName %in% c("Bolonia_thore", "Bolonia_euphrosyne", 
                                                 "Bolonia_selene") ~ "Boloria",
                           scientificName == "Erebia_ligea" ~ "Erebia",
                           scientificName %in% c("Bombus_sp", "Bombus_pratorum", "Bombus_hortorum.jonellus",
                                                 "Bombus_monticola.lapponicus", "Bombus_balteatus","Bombus_lapidarius",
                                                 "Bombus_lucorum.terrestris", "Bombus_pascuorum",
                                                 "Bombus_consobrinus") ~ "Bombus",
                           scientificName == "Apis_mellifera" ~ "Apis"),
         # fix misspelling of Boloria genus
         scientificName = str_replace_all(scientificName, "Bolonia", "Boloria"),
         scientificName = str_replace_all(scientificName, "Bombus_wurfleinii", "Bombus_wurflenii"),
         scientificName = str_replace_all(scientificName, "Bombus_monticola.lapponicus", "Bombus"),
         scientificName = str_replace_all(scientificName, "Bombus_hortorum.jonellus", "Bombus"),
         scientificName = str_replace_all(scientificName, "Bombus_lucorum.terrestris", "Bombus"),
         id = relatedResourceID) |>
  rename(habitat = site)

# Fix the names in the scientificName column
poll_data_correct_names <- poll_data_taxonomy |>
  mutate(scientificName = gsub("_", " ", scientificName),
         scientificName = str_replace_all(scientificName, " sp", ""),
         scientificName = str_replace_all(scientificName, ".moth.", ""))

# Add taxonRank column
poll_data_rank <- poll_data_correct_names |>
  mutate(scientificNameRank = case_when(scientificName %in% c("Lepidoptera", "Araneae") ~ "order",
                                        scientificName %in% c("Brachycera", "Nematocera") ~ "suborder",
                                        scientificName == "Syrphidae" ~ "family",
                                        scientificName == "Bombus" ~ "genus",
                                        scientificName %in% c("Plebejus idas", "Boloria thore",
                                                              "Boloria euphrosyne", "Boloria selene",
                                                              "Erebia ligea", "Bombus pratorum",
                                                              "Bombus balteatus", "Apis mellifera",
                                                              "Bombus lapidarius", "Bombus pascuorum",
                                                              "Bombus consobrinus", "Bombus wurflenii") ~ "species")) |>
  # re-order columns
  select(id, institutionCode, ownerInstitutionCode, basisOfRecord, occurrenceID,
         resourceID, organismQuantity, organismQuantityType, scientificName, 
         resourceRelationshipID, relatedResourceID, relationshipRemarks, resourceScientificName,
         kingdom, phylum, class, order, family, genus, scientificNameRank)


## 2.4. Check if data is publishable with rgbif ----
# Extract dataframe of backbone check 
checked_names <- as.data.frame(name_backbone_checklist(poll_data_rank))

# Extract records where the matchType is not exact
flagged_records <- checked_names |>
  filter(matchType != "EXACT") # only records flagged are those with higher rank - ok!

# Save new data as an occurrence df
write_delim(poll_data_rank, here("data", "pollinator_core.txt"), delim = "\t")

# END OF SCRIPT ----