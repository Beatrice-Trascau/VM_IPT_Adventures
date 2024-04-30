##----------------------------------------------------------------------------##
# IPT_example_1_Budalen_plant_data
# This script contains code to make the eml.xml and metadata.xml files
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
#install.packages("emld")
library(emld)
library(EML)

# 1. DEFINE EML ----
# Define the EML content
eml_data <- list(
  packageId = "418a6571-b6c1-4db0-b90e-8f36bde4c80e/v1.3", # need to change this
  system = "http://gbif.org",
  dataset = list(
    title = "Plant and plant-pollinator interaction from surveys in Budalen, Norway",
    creator = list(
      individualName = list(
        givenName = "Frida",
        surName = "Slettan"
      ),
      organizationName = "University Museum, Norwegian University of Science and Technology",
      address = list(
        city = "Trondheim",
        country = "NO"
      ),
      electronicMailAddress = "fridasle@stud.ntnu.no"
    ),
    creator = list(
      individualName = list(
        givenName = "Beatrice Maria",
        surName = "Trascau"
      ),
      organizationName = "University Museum, Norwegian University of Science and Technology",
      address = list(
        city = "Trondheim",
        country = "NO"
      ),
      electronicMailAddress = "beatrice.m.trascau@ntnu.no"
    ),
    creator = list(
      individualName = list(
        givenName = "James David Mervyn",
        surName = "Speed"
      ),
      organizationName = "University Museum, Norwegian University of Science and Technology",
      address = list(
        city = "Trondheim",
        country = "NO"
      ),
      electronicMailAddress = "james.speed@ntnu.no"
    ),
    pubDate = "2024-05-01",
    language = "eng",
    abstract = list(
      para = "Here we present vegetation data (relative frequencies of flowering plants) and plant-pollinator interaction data collected in Budalen, Norway in 2023."
    ),
    keywordSet = list(
      keyword = c("Samplingevent", "pollination", "low alpine", "scandinavia", "vegetation"),
      keywordThesaurus = "GBIF Dataset Type Vocabulary: http://rs.gbif.org/vocabulary/gbif/dataset_type.xml"
    ),
    intellectualRights = "To the extent possible under law, the publisher has waived all rights to these data and has dedicated them to the Public Domain (CC0 1.0). Users may copy, modify, distribute and use the work, including for commercial purposes, without restriction.",
    coverage = list(
      geographicCoverage = list(
        geographicDescription = "Budalen Valley, Trondelag, Norway",
        boundingCoordinates = list(
          westBoundingCoordinate = 10.55,
          eastBoundingCoordinate = 10.6833,
          northBoundingCoordinate = 62.8,
          southBoundingCoordinate = 62.7
        )
      ),
      temporalCoverage = list(
        rangeOfDates = list(
          beginDate = list(calendarDate = "2023-06-19"),
          endDate = list(calendarDate = "2023-08-02")
        )
      ),
      taxonomicCoverage = list(
        generalTaxonomicCoverage = "Flowering plant species and arthropods surveyed interacting with the flowering plants",
        taxonomicClassification = list(
          taxonRankName = "kingdom",
          taxonRankValue = "Plantae, Animalia"
        )
      )
    ),
    methods = list(
      methodStep = list(description = "See sampling description"),
      sampling = list(
        studyExtent = "Vegetation analysis was conducted in June, July and August 2023",
        samplingDescription = "There were 3 land uses surveyed: Former Haymaking (FH), Extensive Grazing (EG), Intensive Grazing (IG). Each land use had 3 sites, in total 9 sites. In each site 5 transects were surveyed. For the plant survey, 5 quadrats were surveyed along each transect. In each quadrat, the frequency of flowering plants was recorded. The insect survey was done by walking up and down the transects and recording the number of insects flying by, the insects interacting with plants and the plant species they were interacting with."
      )
    ),
    contact = list(
      individualName = list(
        givenName = "Beatrice",
        surName = "Trascau"
      ),
      organizationName = "University Museum, Norwegian University of Science and Technology",
      positionName = "PhD student",
      address = list(
        city = "Trondheim",
        country = "NO"
      ),
      electronicMailAddress = "beatrice.m.trascau@ntnu.no"
    ),
    maintenance = list(
      description = "The dataset is not planned to be updated.",
      maintenanceUpdateFrequency = "notPlanned"
    )
  ),
  additionalMetadata = list(
    metadata = list(
      gbif = list(
        dateStamp = "2016-02-26T01:36:27.311+00:00", # change data stamp
        hierarchyLevel = "dataset",
        citation = "Speed J D M, Trascau B M, Slettan F (2024): Plant and plant-pollinator interaction from surveys in Budalen, Norway. v1. NTNU University Museum."
      )
    )
  )
)

# Convert to EML object
eml_object <- as_emld(eml_data)

# Validate the EML object
emld::eml_validate(eml_object)

# Serialize to XML
eml_xml <- emld::eml_serialize(eml_object, file = "eml.xml")


