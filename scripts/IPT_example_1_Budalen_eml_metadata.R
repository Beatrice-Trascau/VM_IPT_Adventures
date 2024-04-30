##----------------------------------------------------------------------------##
# IPT_example_1_Budalen_plant_data
# This script contains code to make the eml.xml and metadata.xml files
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
#install.packages("emld")
library(emld)

# 1. DEFINE EML ----
# Define the EML content
eml_data <- list(
  packageId = "urn:uuid:your-unique-identifier-here",
  system = "GBIF",
  dataset = list(
    title = "Budalen_2023_Plant_Pollinator_Survey",
    creator = list(
      individualName = list(
        givenName = "Frida",
        surName = "Slettan"
      ),
      electronicMailAddress = "fridasle@stud.ntnu.no",
      organizationName = "University Museum, Norwegian University of Science and Technology"
    ),
    pubDate = "2024-01-01",
    language = "en",
    abstract = list(
      para = "Plant and pollinator surveys from Budalen in 2023"
    ),
    keywordSet = list(
      keyword = c("plant", "pollinators", "interactions")
    ),
    intellectualRights = "Public Domain (CC0 1.0)",
    contact = list(
      individualName = list(
        givenName = "Beatrice Maroa",
        surName = "Trascau"
      ),
      electronicMailAddress = "beatrice.m.trascau@ntnu.no",
      organizationName = "University Museum, Norwegian University of Science and Technology"
    )
  )
)

# Convert the list to an EML object
eml_object <- as_emld(eml_data)

