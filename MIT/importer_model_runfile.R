## Creating the features files for the importers model.
# This code will compile all of the features variables that will be fed to the
# importers model. The features for this model will be built PER YEAR cumlatively, 
# the features for each year will be housed in seperate CSV files, so the number of
# CSV files created on output is determined by the number of unique years seen 
# within the FDA commodity shipping file. 
# e.g. if the FDA shipping file contains shipments that span 2007 - 2015, then 
# "file_2011" will contain features computed for years 2007 - 2011.

## Load Packages and Read in Data ----
print("Installing/loading required packages")
for (package in c('readr', 'dplyr', 'RJSONIO', 'geosphere', 'ggmap')) {
  if (suppressPackageStartupMessages(!require(package, 
                                              character.only = T, quietly = T))) {
    install.packages(package)
    suppressPackageStartupMessages(library(package, 
                                           character.only = T, quietly = T))
  }
}

# Turn off "print col types" when reading in the raw data.
# Turn off other warnings.
options("readr.num_columns" = 0)
oldw <- getOption("warn")
options(warn = -1)

# Load required custom functions and column index elements.
source("~/importer_model_functions.R")
source("~/importer_model_col_indices.R")

# Read in the raw data
load("~/consignee_prod_div.RData")
load("~/shipdf_featdf_post_manu_model.RData")
load("~/us_port_GC.RData")
insp <- read_csv("~/inspections.csv")

# Create Variables ----
print("Creating variables")
# For every shipment in shipdf, get the distance from importer city to the US port.
# Append the distances as a new column in shipdf.
impGC <- getGCimp(shipdf, fdaImpCity)
shipdf$`distance importer to US port` <- 
  mapply(function(x, y) 
    getDistImpUSport(x, y, impGC, usportGC), 
    shipdf[[fdaImpCity]], shipdf[[fdaUSport]], USE.NAMES = FALSE)

# Create vector of the unique shipment years seen within shipdf.
years <- unique(as.numeric(format(shipdf[[fdaDate]], "%Y")))

# Create empty df that will house the shipdf subsets within the loop below.
shipsub <- data_frame()

## For Loop to Build Each Feature ----
# Each loop iteration corresponds to a year.
for (p in years) {
  if (p == years[1]) {
    print(paste("Now computing features for year", p))
  } else {
    print(paste("Now computing features for years", years[1], "thru", p))
  }
  # Create subset dataframe of shipdf basesd on year 1 thru year p.
  shipsub <- rbind(shipsub, shipdf[as.numeric(format(shipdf[[fdaDate]], "%Y")) == p & 
                                     !is.na(shipdf[[fdaImporter]]) & 
                                     !grepl("[Nn]ot [Ss]upplied", shipdf[[fdaImporter]]), ])
  
  ## Create the Features Dataframe ----
  # For now this is merely every unique importer name. Features will be 
  # appended to this dataframe  with the functions below.
  featsub <- data_frame(importer = unique(shipsub[[fdaImporter]]))
  
  ## Get Counts ----
  # For each importer within featsub, get total number manufacturers they 
  # work with, and number of consignees they work with. Record the results 
  # into two new variables within featsub.
  featsub <- getCounts(shipsub, fdaManu, fdaImporter, fdaConsign, featsub, 
                       featImporter)
  
  ## Fraction of weight coming from mfq that are also shippers ----
  # For each importer within featsub, get the fraction of total shipment weight
  # that comes from shipments in which the manufacturer is also acting as the
  # shipper.
  featsub <- kgFromManuShipperMatches(shipsub, fdaImporter, fdaManu, fdaShipper, 
                                      featsub, featImporter)
  
  ## Normalized Yearly Average Weight ----
  # For each importer within featsub, get the mean (over the years) of the 
  # importers shipment weights normalized by the average weight of all 
  # shipments during each year.
  featsub <- getNormWeight(shipsub, fdaDate, fdaKG, featsub, featImporter)
  
  ## Number of Unique US Ports ----
  # For each importer within featsub, get count of their unique US ports from 
  # shipsub.
  featsub <- getUSPorts(shipsub, fdaImporter, fdaUSport, featsub, featImporter)
  
  ## Total weight below 10% quantile ----
  # For each importer within featsub, this is a binary variable that indicates 
  # if the total weight received by the importer falls in the bottom 10% of 
  # all total weights received by all importers.
  featsub <- getExperience(shipsub, fdaImporter, fdaKG, featsub, featImporter)
  
  ## Avg Distances (Manufacturers, Places of Receipt, Foreign Ports) ----
  # For each importer within featsub, this calculates:
  # 1. Mean distance between mfq and place of receipt across the all shipments.
  # 2. Mean distance between place of receipt and foreign port across all shipments.
  # 3. Fraction of transnational shipments between mfg and place of receipt.
  # 4. Fraction of transnational shipments between place of receipt and foreign port.
  featsub <- getAvgDistances(shipsub, fdaManu, fdaImporter, fdaCountry, 
                             fdaPorCountry, fdaFPcountry, featsub, featImporter, 
                             featdf, featManu, featDist1, featDist2)
  
  ## Avg Distance Importer to US Port ----
  # For each importer within featsub, get the mean distance between the importer
  # and the US port for each of their shipments.
  featsub <- getAvgDistImporterUSport(shipsub, fdaImporter, fdaKG, featsub, 
                                      featImporter)
  
  ## Variation of Num of Shipments & Num Manufacturers Across Years ----
  # For each importer in featsub, get the variation of number of shipments and 
  # variation of number of manufacturers they worked with across all years in 
  # which that importer was active.
  featsub <- getVariations(shipsub, fdaDate, fdaImporter, fdaManu, featsub, 
                           featImporter)
  
  ## Dispersion of total weight per Manufacturer ----
  # For each importer, start with list of manufacturers they've worked with in 
  # shipsub, then for each of those manufacturers we calculate the 
  # cumulative shipment weight sent to the importer by the manufacturer. We then 
  # normalize these values by the total weight shipped by the importer so 
  # they represent the fraction of volume each manufacturer ships. We then 
  # get the sum of these normalized values, multiply it by -1, and the product is
  # the dispersion of the total shipment weight per manufacturer for a given importer.
  featsub <- getManuDispersion(shipsub, fdaImporter, fdaManu, fdaKG, featsub, 
                               featImporter)
  
  ## Fraction of Shipments from China ---- 
  # For each importer within featsub, get the fraction of their shipments
  # that came from a manufacturer in China.
  featsub <- getChineseShipments(shipsub, fdaCountry, fdaImporter, featsub, 
                                 featImporter)
  
  ## Number of Sampled Shipments ----
  # For each importer within featsub, get the log of the total number of 
  # sampled shipments.
  featsub <- getSampled(shipsub, fdaImporter, fdaSamp, featsub, featImporter)
  
  ## Number of Refused Shipments ----
  # For each importer within featsub, get the log of the total number of 
  # refused shipments.
  featsub <- getRefused(shipsub, fdaImporter, fdaRefuse, featsub, featImporter)
  
  ## Binary Inspection Variables ----
  # For each importer within featsub, apply a label indicating:
  # 1. Inspected prior to year p.
  # 2. Inspection in year p.
  # 3. Bad inspection in year p (VAI or OAI or RTS).
  featsub <- getBinaryInspVars(insp, inspDate, inspResult, featsub, 
                               featImporter, p)
  
  ## Product Diversity ---- 
  # For each importer within featsub, look name up within consign_prod_div 
  # (consignees names with product diversity generated from running 
  # consignee_model_runfile.R), get mean of product diversity integers of all 
  # of the rows that match. That mean is the product diversity score for that
  # importer.
  featsub <- getProductDiv(featsub, featImporter, consign_prod_div)
  
  # Write featsub to CSV ----
  fileName <- paste0("~/importer_features_", p, ".csv")
  write_csv(featsub, fileName)
}

# Turn warnings back on.
options(warn = oldw)

