## Creating the features files for the consignees model.
# This code will compile all of the features variables that will be fed to the
# consignees model. The features for this model will be built PER YEAR cumlatively, 
# the features for each year will be housed in seperate CSV files, so the number of
# CSV files created on output is determined by the number of unique years seen 
# within the FDA commodity shipping file. 
# e.g. if the FDA shipping file contains shipments that span 2007 - 2015, then 
# "file_2011" will contain features computed for years 2007 - 2011.

## Load Packages and Read in Data ----
print("Installing/loading required packages")
for (package in c('readr', 'dplyr', 'ggmap', 'RJSONIO', 'geosphere', 'tm', 'SnowballC')) {
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
source("consignee_model_functions.R")
source("consignee_model_col_indices.R")

# Read in the raw data
load("shipdf_featdf_post_manu_model.RData")
insp <- read_csv("inspections.csv")
# URL for scraped IG data: https://drive.google.com/file/d/0B73e_vB3Dl62aVV6djZxSnNqU2c/view?usp=sharing
# Save the csv from Google Drive to ~/fda_project/consignee_model, then run:
productIG <- read_csv("IG_products_5k.csv")

# Create Variables ----
print("Creating variables")
# For every shipment in shipdf, get the distance from consignee city to the US port.
# Append the distances as a new column in shipdf.
consignGC <- getGCconsign(shipdf, fdaConCity)
usportGC <- getGCusport(shipdf, fdaUSport)

porGC <- getGCpor(shipdf, fdaPOR)
fpGC <- getGCfp(shipdf, fdaFP)
porGC <- getPorCountry(shipdf, porGC)
fpGC <- getFpCountry(shipdf, fpGC)

shipdf$`distance consignee to US port` <- 
  mapply(function(x, y) 
    getDistConsignUSport(x, y, consignGC, usportGC), 
    shipdf[[fdaConCity]], shipdf[[fdaUSport]], USE.NAMES = FALSE)


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
                                     !is.na(shipdf[[fdaConsign]]) & 
                                     !grepl("[Nn]ot [Ss]upplied", shipdf[[fdaConsign]]), ])
  
  ## Create the Features Dataframe ----
  # For now this is merely every unique consignee name. Features will be 
  # appended to this dataframe  with the functions below.
  featsub <- data_frame(consignee = unique(shipsub[[fdaConsign]]))
  
  ## Get Counts ----
  # For each consignee within featsub, get total number manufacturers they 
  # work with, and number of importers they work with. Record the results 
  # into two new variables within featsub.
  featsub <- getCounts(shipsub, fdaManu, fdaConsign, fdaImporter, featsub, featConsign)
  
  ## Fraction of weight coming from mfq that are also shippers ----
  # For each consignee within featsub, get the fraction of total shipment weight
  # that comes from shipments in which the manufacturer is also acting as the
  # shipper.
  featsub <- kgFromManuShipperMatches(shipsub, fdaConsign, fdaManu, fdaShipper, 
                                      featsub, featConsign)
  
  ## Normalized Yearly Average Weight ----
  # For each consignee within featsub, get the mean (over the years) of the 
  # consignees shipment weights normalized by the average weight of all 
  # shipments during each year.
  featsub <- getNormWeight(shipsub, fdaDate, fdaKG, featsub, featConsign)
  
  ## Number of Unique US Ports ----
  # For each consignee within featsub, get count of their unique US ports from 
  # shipsub.
  featsub <- getUSPorts(shipsub, fdaConsign, fdaUSport, featsub, featConsign)
  
  ## Total weight below 10% quantile ----
  # For each consignee within featsub, this is a binary variable that indicates 
  # if the total weight received by the consignee falls in the bottom 10% of 
  # all total weights received by all consignees.
  featsub <- getExperience(shipsub, fdaConsign, fdaKG, featsub, featConsign)
  
  ## Avg Distances (Manufacturers, Places of Receipt, Foreign Ports) ----
  # For each consignee within featsub, this calculates:
  # 1. Mean distance between mfq and place of receipt across the all shipments.
  # 2. Mean distance between place of receipt and foreign port across all shipments.
  # 3. Fraction of transnational shipments between mfg and place of receipt.
  # 4. Fraction of transnational shipments between place of receipt and foreign port.
  featsub <- getAvgDistances(shipsub, fdaManu, fdaConsign, fdaCountry, 
                             fdaPorCountry, fdaFPcountry, featsub, featConsign, 
                             featdf, featManu, featDist1, featDist2)
  
  ## Avg Distance Consignee to US Port ----
  # For each consignee within featsub, get the mean distance between the consignee
  # and the US port for each of their shipments.
  featsub <- getAvgDistConsignUSport(shipsub, fdaConsign, fdaKG, featsub, featConsign)
  
  ## Variation of Num of Shipments & Num Manufacturers Across Years ----
  # For each consignee in featsub, get the variation of number of shipments and 
  # variation of number of manufacturers they worked with across all years in 
  # which that consignee was active.
  featsub <- getVariations(shipsub, fdaDate, fdaConsign, fdaManu, featsub, 
                           featConsign)
  
  ## Dispersion of total weight per Manufacturer ----
  # For each consignee, start with list of manufacturers they've worked with in 
  # shipsub, then for each of those manufacturers we calculate the 
  # cumulative shipment weight sent to the consignee by the manufacturer. We then 
  # normalize these values by the total weight shipped by the consignee so 
  # they represent the fraction of volume each manufacturer ships. We then 
  # get the sum of these normalized values, multiply it by -1, and the product is
  # the dispersion of the total shipment weight per manufacturer for a given consignee.
  featsub <- getManuDispersion(shipsub, fdaConsign, fdaManu, fdaKG, featsub, 
                               featConsign)
  
  ## Fraction of Shipments from China ---- 
  # For each consignee within featsub, get the fraction of their shipments
  # that came from a manufacturer in China.
  featsub <- getChineseShipments(shipsub, fdaCountry, fdaConsign, featsub, 
                                 featConsign)
  
  ## Number of Sampled Shipments ----
  # For each consignee within featsub, get the log of the total number of 
  # sampled shipments.
  featsub <- getSampled(shipsub, fdaConsign, fdaSamp, featsub, featConsign)
  
  ## Number of Refused Shipments ----
  # For each consignee within featsub, get the log of the total number of 
  # refused shipments.
  featsub <- getRefused(shipsub, fdaConsign, fdaRefuse, featsub, featConsign)
  
  ## Binary Inspection Variables ----
  # For each consignee within featsub, apply a label indicating:
  # 1. Inspected prior to year p.
  # 2. Inspection in year p.
  # 3. Bad inspection in year p (VAI or OAI or RTS).
  featsub <- getBinaryInspVars(insp, inspDate, inspResult, featsub, 
                               featConsign, p)
  
  ## Product Diversity ----
  # For each consignee within featsub, capture the diversity of the product 
  # portfolio based on shipping info scraped from Import Genius. 
  # problem here featsub <- getProductDiv(featsub, featConsign, productIG)
  
  
  # Write featsub to CSV/RData file ----
  fileName <- paste0("consignee_features_", p, ".csv")
  write_csv(featsub, fileName)
  if (p == years[length(years)]) {
    consign_prod_div <- featsub[, c(1, ncol(featsub))]
    save(consign_prod_div, file = "consignee_prod_div.RData")
    save(usportGC, file = "us_port_GC.RData")
  }
}

# Trun warnings back on.
options(warn = oldw)

