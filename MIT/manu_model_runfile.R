## Load Packages and Read in Data ----
print("Installing/loading required packages")
for (package in c('readr', 'dplyr', 'ggmap', 'geosphere', 'RJSONIO')) {
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
source("~/manu_model_functions.R")
source("~/manu_model_col_indices.R")

# Read in the raw data
ref <- read_csv("~/fda_refusals_refined.csv")
shipdf <- read_csv("~/shrimpsubset5k.csv")
IG <- read_csv("~/IGsubset5k.csv")


## FDA shipping / IG Match ----
# Match observations within IG with those of the FDA commodity shipping dataset.
# This function will add two variables to shipdf, "Place of Receipt" and 
# "Foreign Port". For positive matches, record within shipdf values for 
# these two variables that are seem within the IG df.
# The matching is done with variables "Bill of Lading" OR "Container Number" OR
# ("Carrier Code" AND "Voyage Number").
shipdf <- getMatches(shipdf, fdaManu, fdaDate, fdaBill, fdaContain, 
                     fdaCarr, fdaVoy, fdaCountry, fdaShipType, 
                     IG, igDate, igBill, igContain, igCarr, igVoy, 
                     igPOR, igForeign)

# Remove IG dataframe from R.
rm(IG)

## Create the Features Dataframe ----
# For now this is merely every unique combination of manufacturer name plus 
# manufacturer country. Features will be appended to this dataframe  with the
# functions below.
featdf <- as_data_frame(unique(shipdf[, c(fdaManu, fdaCountry)]))


## Get Counts ----
# For each manufacturer within the featdf, get total number of shipments, 
# number of consignees they work with, and number of shippers they work with. 
# Record the results into three new variables within featdf.
featdf <- getCounts(shipdf, fdaManu, fdaConsign, fdaShipper, featdf, featManu)


## Normalized Yearly Average Weight ----
# Average (over the years) of the manufacturers shipments' weights normalized 
# by the average weight of all shipments during each year.
shipdf <- getKilograms(shipdf, fdaWeight, fdaUnit)
featdf <- getNormWeight(shipdf, fdaDate, fdaKG, fdaManu, featdf, featManu)


## Manufacturer is also Shipper ----
# For each manufacturer, get the fraction of shipments where the manufacturer 
# and the shipper are the same.
featdf <- getManuShipperMatches(shipdf, fdaManu, fdaShipper, featdf, featManu)


## Geocodes ----
# Get geocodes for all manufacturer addresses, Place of Receipts, and 
# Foreign Ports with shipdf. For each of those three, a dataframe is created
# containing all unique values and the associated geocode.
# NOTE: The "getGC" functions use the Data Science Toolkit API to fetch geocodes,
# but for any queries that fail, they will attempt to fetch the geocode from 
# the Google Maps API. Google Maps API per day limit is 2500 queries, 
# with a premium subscription it's 100,000 queries per day.
shipdf <- getFullAddress(shipdf, fdaCity, fdaProv, fdaCountry)
manuGC <- getGCmanu(shipdf, fdaManuAddress)
porGC <- getGCpor(shipdf, fdaPOR)
fpGC <- getGCfp(shipdf, fdaFP)


## Reverse Geocodes ----
# Run geocodes thru a reverse geocode lookup to get country for each 
# Place of Receipt within porGC andForeign Port within fpGC. Append countries 
# as a new column to each df.
# NOTE: These functions exclusively use Google Maps API to fetch addresses, as 
# the Data Science Toolkit API doesn't offer reverse geocode lookups. 
# Google Maps API per day limit is 2500 queries, with a premium subscription 
# it's 100,000 queries per day.
porGC <- getPorCountry(shipdf, porGC)
fpGC <- getFpCountry(shipdf, fpGC)


## Distance Manufacturer - Place of Receipt ----
# For each manufacturer, get the average distance between the manufacturer and 
# the place of receipt across all the manufacturer's shipments.
featdf <- getAvgDistManuPOR(shipdf, fdaManu, fdaManuAddress, fdaPOR, 
                            manuGC, porGC, featdf, featManu)


## Transnational Manufacturer - Place of Receipt ----
# For each manufacturer, get the fraction of shipments where the manufacturer 
# and the place of receipt are located in different countries.
featdf <- getTransManuPor(shipdf, fdaManu, fdaCountry, fdaPOR, featdf, 
                          featManu, porGC)


## Distance Place of Receipt - Foreign Port ----
# For each manufacturer, get the average distance between the place of receipt 
# and the foreign port across all the manufacturer's shipments.
featdf <- getAvgDistPorFp(shipdf, fdaManu, fdaFP, fdaPOR, fpGC, porGC, 
                          featdf, featManu)


## Transnational Place of Receipt - Foreign Port ----
# For each manufacturer, get the fraction of shipments where the place of 
# receipt and the foreign port are located in different countries.
featdf <- getTransPorFp(shipdf, fdaManu, fdaPOR, fdaFP, 
                        featdf, featManu, porGC, fpGC)


## Variation of Number of Consignees Across Years ----
# For each manufacturer, get the coefficient of variation 
# (standard deviation divided by mean) of the number of unique consignees the 
# manufacturer works with each year.  This feature captures variability in the 
# yearly consignee number for the manufacturer. Note that the coefficient of 
# variation is computed between the year of the first shipment of the manufacturer 
# until the year of the last shipment of the manufacturer.
featdf <- getConsignVar(shipdf, fdaConsign, fdaDate, fdaManu, featdf, featManu)

## Variation of number of shipments across years ----
# For each manufacturer, get the Coefficient of variation 
# (standard deviation divided by mean) of the number of shipments across year 
# of each manufacturer.  This feature captures variability in the yearly number 
# of shipments for the manufacturer. Note that the coefficient of variation is 
# computed between the year of the first shipment of the manufacturer until the 
# year of the last shipment of the manufacturer.
featdf <- getShipmentsVar(shipdf, fdaDate, fdaManu, featdf, featManu)


## Is Manufacturer in One of these Countries. ----
# For each manufacturer, indicate whether they are located in any of these countries:
# Bangladesh, China, India, Indonesia, Vietnam, Malaysia, Thailand.
featdf <- specificCountries(shipdf, fdaManu, fdaCountry, featdf, featManu)


## Dispersion of total shipment weight per consignee ----
# For each manufacturer, start with list of consignees they've worked with in 
# shipdf, then for each consignee of the manufacturer we calculate the 
# cumulative shipment weight sent to the consignee by the manufacturer. We then 
# normalize these values by the total weight shipped by the manufacturer so 
# they represent the fraction of volume each consignee receives. We then 
# get the sum of these normalized values, multiply it by -1, and the product is
# the dispersion of the total shipment weight per consignee for a given manufacturer.
featdf <- getConsigneeDispersion(shipdf, fdaConsign, fdaManu, fdaKG, 
                                 featdf, featManu)


## Fraction of Shipments to "Inexperienced" Consignees ----
# For each manufacturer, get fraction of shipments sent to "inexperienced" consignees; 
# these are defined as consignees with total received weight in the bottom 10% 
# of all consignees in the shipdf dataset.
featdf <- getConsigneeExperience(shipdf, fdaConsign, fdaManu, fdaKG, 
                                 featdf, featManu)


## Fraction of Sampled Shipments & Total Number of Sampled Shipments ----
# For each manufacturer, get the fraction of sampled shipments within shipdf.
featdf <- getNumSampled(shipdf, fdaManu, fdaSamp, featdf, featManu)


## Intentional vs Unintentional Adulteration for Refused Shipments ----
# For each manufacturer, get the fraction of shipments from shipdf that were
# refused due to intentional adulteration and unintentional adulteration, 
# each appended as a seperate new column to featdf.
featdf <- getIntentFractions(shipdf, fdaManu, fdaDate, fdaRefuse, fdaSamp, 
                             featdf, featSamp, featNshipments, featManu, 
                             ref, refDate, refManu, refCode)

# Write objects to output files ----
options(warn = oldw)
write_csv(featdf, "~/manu_features.csv")
save(shipdf, featdf, file = "~/shipdf_featdf_post_manu_model.RData")
