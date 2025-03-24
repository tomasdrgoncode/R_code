## Distance Consignee to US Port ----
# Functions: getLatLong, getGCimp, getGCusport, getDistConsignPort
# For every shipment in shipdf, get the distance from consignee city to US port.

# Get geocodes for every consignee address in shipdf.
getGCimp <- function(shipdf, fdaImpCity) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaImpCity = col index of consignee cities within shipdf.
  # Output is 2 col df listing every consignee city and its geocode.
  
  uniaddress <- unique(shipdf[[fdaImpCity]][!is.na(shipdf[[fdaImpCity]])])
  
  # Use method "dsk" to get geocode for every unique value in address.
  gc <- geocode(uniaddress, source = "dsk", messaging = FALSE)
  impGC <- cbind(gc, uniaddress)
  impGC$uniaddress <- as.character(impGC$uniaddress)
  impGC$lon <- unlist(impGC$lon)
  impGC$lat <- unlist(impGC$lat)
  
  # For all NA's returned during method "dsk", use google maps API to try to 
  # fill in those gaps.
  ids <- unique(c(which(is.na(impGC$lon)), which(is.na(impGC$lat))))
  if (length(ids) > 0) {
    goog <- sapply(ids, function(x) 
      geocode(impGC[x, ]$uniaddress, source = "google", messaging = FALSE))
    if (length(goog) > 0 && length(goog) == (length(ids) * 2)) {
      impGC[ids, ]$lon <- goog[seq(1, length(goog), 2)]
      impGC[ids, ]$lat <- goog[seq(2, length(goog), 2)]
    }
  }
  
  return(impGC)
}


# For each row in shipdf, calculate distance between the US Port and 
# Importer city.
getDistImpUSport <- function(impAdd, usport, impGC, usportGC) {
  # Params:
  # 
  # Output is
  if (is.na(impAdd) || is.na(usport)) {
    return(NA)
  }
  usport_gc <- unname(unlist(usportGC[match(usport, usportGC$uniaddress), c(1, 2)]))
  imp_gc <- unname(unlist(impGC[match(impAdd, impGC$uniaddress), c(1, 2)]))
  if (any(is.na(usport_gc) || any(is.na(imp_gc)))) {
    return(NA)
  }
  
  distance <- as.vector(distm(usport_gc, imp_gc, fun = distHaversine)/ 1000)
  return(distance)
}


## Get Counts: num manufacturers, num importers ----
# For each importer, this function will get: total number of manufacturers 
# they work with, and number of importers they work with.
getCounts <- function(shipsub, fdaManu, fdaImporter, fdaConsign, featsub, 
                      featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipsub.
  # fdaImporter = col index of importer names within shipsub.
  # fdaConsign = col index of consignee names within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 2 new columns ("numManufacturers", "numImporters")
  print("Now computing getCounts (num mfq & num importers...")
  
  featsub$numManufacturers <- NA
  featsub$numImporters <- NA
  for (i in featsub[[featImporter]]) {
    manu <- length(unique(shipsub[shipsub[[fdaImporter]] == 
                                      i, ][[fdaManu]]))
    featsub[featsub[[featImporter]] == i, ]$numManufacturers <- manu
    
    consign <- length(unique(shipsub[shipsub[[fdaImporter]] == 
                                      i, ][[fdaConsign]]))
    featsub[featsub[[featImporter]] == i, ]$numImporters <- consign
  }
  return(featsub)
}


## Fraction of weight coming from mfq that are also shippers ----
# For each importer within featsub, get the fraction of total shipment weight
# that comes from shipments in which the manufacturer is also acting as the
# shipper.
kgFromManuShipperMatches <- function(shipsub, fdaImporter, fdaManu, fdaShipper, 
                                     featsub, featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaImporter = col index of importer names within shipsub.
  # fdaManu = col index of manufacturer names within shipsub.
  # fdaShipper = col index of shipper names within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column 
  # ("frac fo KG shipped when manu equals shipper")
  
  print("Now computing fraction of weight coming from mfq that are also shippers...")
  featsub$`frac of KG shipped when manu equals shipper` <- NA
  # Create list of lists that contain, for each importer within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featImporter]], function(x) which(shipsub[[fdaImporter]] %in% x))
  # Get vector of the total weight shipped by each importer within featsub.
  weights <- unname(sapply(ids, function(x) sum(shipsub[unlist(x), ][[fdaKG]])))
  # Get vector of the total weight shipped by each importer within featsub but 
  # only for rows within shipdf in which the manufacturer == shipper.
  weightsub <- unname(
    sapply(ids, function(x) 
      sum(shipsub[unlist(x), ][[fdaKG]][shipsub[[fdaManu]][unlist(x)] == 
                                          shipsub[[fdaShipper]][unlist(x)]])))
  # Divide each element of weightsub by those of weights, append products to 
  # featsub.
  featsub$`frac of KG shipped when manu equals shipper` <- weightsub / weights
  
  return(featsub)
}

  
## Normalized Yearly Average Weight ----
# For each importer within featsub, get the mean (over the years) of the 
# importers shipment weights normalized by the average weight of all 
# shipments during each year.
getNormWeight <- function(shipsub, fdaDate, fdaKG, featsub, featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaDate = col index of dates within shipsub.
  # fdaKG = col index of weight in KG within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("normalizedweight").
  print("Now computing Normalized Yearly Average Weight...")
  
  # Create table of each unique year found within shipsub, each row includes year
  # and mean of all weights (in KG) for that year.
  avgs <- vector()
  uniyears <- unique(as.numeric(format(shipsub[[fdaDate]], "%Y")))
  for (i in uniyears) {
    avgs <- c(avgs, 
              mean(
                shipsub[as.numeric(format(shipsub[[fdaDate]], "%Y")) == i, ][[fdaKG]], 
                na.rm = TRUE))
  }
  weightsdf <- data_frame(years = uniyears, mean = avgs)
  # For every observation in shipsub, take value weightKG and divide by the mean 
  # from weightsdf that corresponds with the year of the observation from 
  # shipsub, save output in vector normweight.
  normweight <- vector()
  for (i in seq_len(nrow(shipsub))) {
    x <- weightsdf[weightsdf$years == 
                     as.numeric(format(shipsub[[fdaDate]][i], "%Y")), ]$mean
    normweight <- c(normweight, (shipsub[[fdaKG]][i] / x))
  }
  # For each manufacturer, get mean of all the values compiled within normweight, 
  # append the output to all of the manufacturer's observations within shipsub.
  featsub$normalizedWeight <- NA
  for (i in featsub[[featImporter]]) {
    id <- which(shipsub[[fdaImporter]] %in% i)
    x <- mean(normweight[id], na.rm = TRUE)
    if (!is.na(x) && x < 200) {
      featsub[featsub[[featImporter]] == i, ]$normalizedWeight <- x
    } else {
      featsub[featsub[[featImporter]] == i, ]$normalizedWeight <- NaN
    }
  }
  return(featsub)
}


## Number of Unique US Ports ----
# For each importer within featsub, get count of unique US ports from shipsub.
getUSPorts <- function(shipsub, fdaImporter, fdaUSport, featsub, featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaImporter = col index of importer names within shipsub.
  # fdaUSport = col index of US ports within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("numUSports")
  print("Now computing number of US ports...")
  featsub$numUSports <- unname(
    sapply(featsub[[featImporter]], function(x) 
      length(
        unique(shipsub[which(shipsub[[fdaImporter]] %in% x), ][[fdaUSport]]))))
  
  return(featsub)
}


## Total weight below 10% quantile ----
# For each importer within featsub, this is a binary variable that indicates 
# if the total weight received by the importer falls in the bottom 10% of 
# all total weights received by all importers.
getExperience <- function(shipsub, fdaImporter, fdaKG, featsub, featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaImporter = col index of importer names within shipsub.
  # fdaKG = col index of weight in KG within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("importer inexperienced")
  print("Now computing total weight below 10% quantile...")
  # Create list of lists that contain, for each importer within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featImporter]], function(x) which(shipsub[[fdaImporter]] %in% x))
  # Get vector of the total weight shipped by each importer within featsub.
  weights <- unname(sapply(ids, function(x) sum(shipsub[unlist(x), ][[fdaKG]])))
  # Create binary vector indicating, for each importer, whether their total 
  # weight shipped is in the bottom 10 percentile of all importers. Append this 
  # vector to featsub.
  featsub$`importer inexperienced` <- unname(
    ifelse(weights > quantile(weights, 0.10, na.rm = TRUE), 0, 1))
  
  return(featsub)
}


## Avg Distances ----
# For each importer within featsub, this calculates:
# 1. Mean distance between mfq and place of receipt across the all shipments.
# 2. Mean distance between place of receipt and foreign port across all shipments.
# 3. Fraction of transnational shipments between mfg and place of receipt.
# 4. Fraction of transnational shipments between place of receipt and foreign port.
getAvgDistances <- function(shipsub, fdaManu, fdaImporter, fdaCountry, 
                            fdaPorCountry, fdaFPcountry, featsub, featImporter, 
                            featdf, featManu, featDist1, featDist2) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipsub.
  # fdaImporter = col index of importer names within shipsub.
  # fdaCountry = col index of manufacturer countries within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # featdf = complete features dataframe.
  # featDist1 = col index of `dist manu - place of recepit` within featdf.
  # featDist2 = col index of `dist place of receipt - foreign port` within featdf.
  # Output is featsub with 4 new columns: 
  # "avg dist manu - place of receipt"
  # "avg dist place of receipt - foreign port"
  # "transnatl manu - place of receipt"
  # "transnatl place of receipt - foreign port"
  print("Now computing getAvgDistances...")
  featsub$`avg dist manu - place of receipt` <- NA
  featsub$`avg dist place of receipt - foreign port` <- NA
  featsub$`transnatl manu - place of receipt` <- NA
  featsub$`transnatl place of receipt - foreign port` <- NA
  # Create binary vector indicating, for each row of shipsub, whether 
  # manu country == Place of Receipt country
  trans_manu_por <- vector()
  for (i in seq_len(nrow(shipsub))) {
    if (any(is.na(shipsub[[fdaCountry]][[i]])) || 
        any(is.na(shipsub[[fdaPorCountry]][[i]]))) {
      trans_manu_por <- c(trans_manu_por, NA)
    } else {
      trans_manu_por <- c(trans_manu_por, 
                          ifelse(shipsub[[fdaCountry]][[i]] == 
                                   shipsub[[fdaPorCountry]][[i]], 0, 1))
    }
  }
  # Create binary vector indicating, for each row of shipsub, whether 
  # Place of Receipt country == Foreign Port country
  trans_fp_por <- vector()
  for (i in seq_len(nrow(shipsub))) {
    if (any(is.na(shipsub[[fdaFPcountry]][[i]])) || 
        any(is.na(shipsub[[fdaPorCountry]][[i]]))) {
      trans_fp_por <- c(trans_fp_por, NA)
    } else {
      trans_fp_por <- c(trans_fp_por, 
                        ifelse(shipsub[[fdaFPcountry]][[i]] == 
                                 shipsub[[fdaPorCountry]][[i]], 0, 1))
    }
  }
  
  for (i in seq_len(nrow(featsub))) {
    id <- which(shipsub[[fdaImporter]] %in% featsub[[featImporter]][i])
    manus <- unique(shipsub[id, ][[fdaManu]])
    id2 <- which(featdf[[featManu]] %in% manus)
    featsub$`avg dist manu - place of receipt`[i] <- 
      mean(featdf[id2, ][[featDist1]], na.rm = TRUE)
    featsub$`avg dist place of receipt - foreign port`[i] <- 
      mean(featdf[id2, ][[featDist2]], na.rm = TRUE)
    featsub$`transnatl manu - place of receipt`[i] <- 
      mean(trans_manu_por[id], na.rm = TRUE)
    featsub$`transnatl place of receipt - foreign port`[i] <- 
      mean(trans_fp_por[id], na.rm = TRUE)
  }
  return(featsub)
}



## Avg Distance Importer City to US Port ----
# For each importer within featsub, get the mean distance between the importer
# and the US port for each of their shipments.
getAvgDistImporterUSport <- function(shipsub, fdaImporter, fdaKG, featsub, 
                                    featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaImporter = col index of importer names within shipsub.
  # fdaKG = col index of weights in KG within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("distance importer - US port")
  print("Now computing avg distance importer city to US port...")
  featsub$`distance importer - US port` <- NA
  for (i in featsub[[featImporter]]) {
    id <- which(shipsub[[fdaImporter]] %in% i)
    featsub[featsub[[featImporter]] == i, ]$`distance importer - US port` <- 
      log10(mean(shipsub[id, ][[fdaKG]], na.rm = TRUE))
  }
  
  # Rewrite all results of "-Inf" to be "0".
  if (any(is.infinite(featsub$`distance importer - US port`))) {
    featsub[is.infinite(
      featsub$`distance importer - US port`), ]$`distance importer - US port` <- 0
  }
  return(featsub)
}


## Variation of Num of Shipments and Num of Manufacturers Across Years ----
# For each importer in shipsub, get the variation of number of shipments 
# across all years in which that importer was active.
getVariations <- function(shipsub, fdaDate, fdaImporter, featManu, featsub, 
                          featImporter) {
  # Params:
  # shipsub = dataframe, FDA shipping data.
  # fdaDate = col index of the ship date within shipsub.
  # fdaImporter = col index of the conignee names within shipsub.
  # fdaManu = col index of the manufacturer names within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("shipment variation")
  print("Now computing variation of num shipments & num manufacturers...")
  
  # Start by getting all years in which the importer was active
  # i.e. had shipments, and the number of shipments in each of those years.
  featsub$`shipment variation` <- NA
  featsub$`manufacturer variation` <- NA
  shipsub$year <- as.numeric(format(shipsub[[fdaDate]], "%Y"))
  for (i in featsub[[featImporter]]) {
    yr <- unique(shipsub[shipsub[[fdaImporter]] == i, ]$year)
    nship <- unname(
      sapply(yr, function(x) 
        nrow(shipsub[shipsub[[fdaImporter]] == i & shipsub$year == x, ])))
    nmanu <- unname(
      sapply(yr, function(x) 
        length(unique(shipsub[shipsub[[fdaImporter]] == i & 
                                shipsub$year == x, ][[fdaManu]]))))
    # Edit the year, num shipment and num manu vectors to fill in any data 
    # missing due to inactivity.
    # e.g. yr = "2008, 2009, 2012" and nship = "6, 10, 8" would become
    # yr = "2008, 2009, 2010, 2011, 2012" and nship = "6, 10, 0, 0, 8"
    if (length(yr) > 1) {
      yrfull <- yr[1]:yr[length(yr)]
      nshipfull <- vector()
      nmanufull <- vector()
      for (k in seq_len(length(yrfull))) {
        if (any(yr == yrfull[k])) {
          id <- grep(yrfull[k], yr)
          nshipfull[k] <- nship[id]
          nmanufull[k] <- nmanu[id]
        } else {
          nshipfull[k] <- 0
          nmanufull[k] <- 0
        }
      }
    } else {
      yrfull <- yr
      nshipfull <- nship
      nmanufull <- nmanu
    }
    # Last, for both num shipments and num manufacturers, compute the mean 
    # per year, the standard deviation, and the coefficient of variation 
    # (CoV gets recorded to featsub).
    if (length(yrfull) == 1) {
      featsub[featsub[[featImporter]] == i, ]$`shipment variation` <- 0
      featsub[featsub[[featImporter]] == i, ]$`manufacturer variation` <- 0
    } else {
      avgShip <- sum(nshipfull) / ((yrfull[length(yrfull)] - yrfull[1]) + 1)
      avgManu <- sum(nmanufull) / ((yrfull[length(yrfull)] - yrfull[1]) + 1)
      numeratorShip <- vector()
      numeratorManu <- vector()
      for (t in seq_len(length(yrfull))) {
        numeratorShip <- c(numeratorShip, (nshipfull[t] - avgShip)^2)
        numeratorManu <- c(numeratorManu, (nmanufull[t] - avgManu)^2)
      }
      SD <- sqrt(sum(numeratorShip) / (yrfull[length(yrfull)] - yrfull[1]) + 10e-6)
      featsub[featsub[[featImporter]] == i, ]$`shipment variation` <- SD / avgShip
      SD <- sqrt(sum(numeratorManu) / (yrfull[length(yrfull)] - yrfull[1]) + 10e-6)
      featsub[featsub[[featImporter]] == i, ]$`manufacturer variation` <- SD / avgManu
    }
  }
  return(featsub)
}


## Dispersion of Total Shipment Weight per Manufacturer ----
# For each importer, get the dispersion of the total shipment weight per 
# manufacturer that the importer has worked with.
# This dispersion calculation is a measure of entropy.
getManuDispersion <- function(shipsub, fdaImporter, fdaManu, fdaKG, 
                                   featsub, featImporter) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaImporter = col index of the importer names within shipdf.
  # fdaManu = col index of the manufacturer names within shipdf.
  # fdaKG = col index of weights in KG within shipdf.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("manufacturer dispersion")
  print("Now computing dispersion of total shipment weight per mfq...")
  
  featsub$`manufacturer dispersion` <- NA
  for (i in seq_len(nrow(featsub))) {
    if (featsub$numManufacturers[i] == 0) {
      featsub$`manufacturer dispersion`[i] <- 0
      next
    }
    df <- shipsub[shipsub[[fdaImporter]] == featsub[[featImporter]][i] & 
                        !is.na(shipsub[[fdaImporter]]), c(fdaImporter, fdaManu, fdaKG)]
    df <- df[complete.cases(df), ]
    normweight <- vector()
    for (n in unique(df[[2]])) {
      x <- sum(df[df[[2]] == n, ][[3]]) / sum(df[[3]])
      normweight <- c(normweight, x*log(x))
    }
    featsub$`manufacturer dispersion`[i] <- -sum(normweight)
  }
  return(featsub)
}


## Fraction of Shipments from China ---- 
# For each importer within featsub, get the fraction of their shipments
# that came from a manufacturer in China.
getChineseShipments <- function(shipsub, fdaCountry, fdaImporter, featsub, 
                                featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaCountry = col index of manufacturer countries within shipsub.
  # fdaImporter = col index of importer names within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("chineseShipments").
  print("Now computing fraction of shipments from china...")
  
  # Create list of lists that contains, for each importer within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featImporter]], function(x) which(shipsub[[fdaImporter]] %in% x))
  # Get vector of the total num of shipments by each importer within featsub.
  nship <- unname(sapply(ids, function(x) length(unlist(x))))
  # Get vector of num of Chinese shipments for each importer within featsub.
  chinese <- unname(
    sapply(ids, function(x) 
      sum(shipsub[[fdaCountry]][unlist(x)] == "China")))
  featsub$chineseShipments <- chinese / nship
  return(featsub)
}


## Number of Sampled Shipments ----
# For each importer within featsub, get the log of the total number of 
# sampled shipments.
getSampled <- function(shipsub, fdaImporter, fdaSamp, featsub, featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaImporter = col index of importer names within shipsub.
  # fdaSamp = col index of sampling info within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("numSampled")
  print("Now computing number of sampled shipments...")
  
  # Create list of lists that contains, for each importer within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featImporter]], function(x) which(shipsub[[fdaImporter]] %in% x))
  
  binarySamp <- ifelse(!is.na(shipsub[[fdaSamp]]), 1, 0)
  numSamp <- unname(sapply(ids, function(x) sum(binarySamp[unlist(x)])))
  featsub$numSampled <- log10(1 + numSamp)
  return(featsub)
}


## Number of Refused Shipments ----
# For each importer within featsub, get the log of the total number of 
# refused shipments.
getRefused <- function(shipsub, fdaImporter, fdaRefuse, featsub, featImporter) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaImporter = col index of importer names within shipsub.
  # fdaRefuse = col index of refusal info within shipsub.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 1 new column ("numRefused").
  print("Now computing number of refused shipments...")
  
  # Create list of lists that contains, for each importer within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featImporter]], function(x) which(shipsub[[fdaImporter]] %in% x))
  
  binaryRef <- grepl("154|155", shipsub[[fdaRefuse]])
  numRef <- unname(sapply(ids, function(x) sum(binaryRef[unlist(x)])))
  featsub$numRefused <- log10(1 + numRef)
  
  return(featsub)
}


## Binary Inspection Variables ----
# For each importer within featsub, record whether or not they: 
# 1. Were inspected prior to year p.
# 2. Were inspected in year p.
# 3. Had a bad insp in year p (VAI or OAI or RTS).
getBinaryInspVars <- function(insp, inspDate, inspResult, featsub, 
                                featImporter, p) {
  # Params:
  # insp = dataframe, FDA site inspection data.
  # inspDate = col index of inspection dates within insp.
  # inspResult = col index of inspection outcomes within insp.
  # featsub = features dataframe.
  # featImporter = col index of importer names within featsub.
  # Output is featsub with 3 new columns ("insp prior to p", "insp in p", 
  # "bad insp in p")
  print("Now computing binary inspection variables...")
  
  insp$year <- as.numeric(format(insp[[inspDate]], "%Y"))
  # Create list of lists that contains, for each importer within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featImporter]], function(x) which(insp[[inspName]] %in% x))
  
  # Inspected prior to year p.
  prior <- unname(
    sapply(ids, function(x) ifelse(
      length(unlist(x)) == 0, 0, ifelse(any(insp[unlist(x), ]$year < p), 1, 0))))
  colname <- paste0("inspected prior to ", p)
  featsub[[colname]] <- prior
  
  # Inspected in year p.
  current <- unname(
    sapply(ids, function(x) ifelse(
      length(unlist(x)) == 0, 0, ifelse(any(insp[unlist(x), ]$year == p), 1, 0))))
  colname <- paste0("inspected in ", p)
  featsub[[colname]] <- current
  
  # Bad inspection outcome in year p.
  bad <- unname(
    sapply(ids, function(x) 
      ifelse(length(unlist(x)) == 0, 0, length(
        insp[unlist(x), ][[inspResult]][insp$year[unlist(x)] == p & 
                                          grepl("VAI|OAI|RTS", insp[[inspResult]][unlist(x)])]))))
  colname <- paste0("bad inspection in ", p)
  featsub[[colname]] <- ifelse(bad > 0, 1, 0)
  
  return(featsub)
}



## Product Diversity ----
getProductDiv <- function(featsub, featImporter, consign_prod_div) {
  
  # For each importer within featsub, look name up within consign_prod_div 
  # (consignees names with product diversity generated from running 
  # consignee_model_runfile.R), get mean of product diversity integers of all 
  # of the rows that match.
  print("Now computing product diversity...")
  
  output <- vector()
  for (i in seq_len(nrow(featsub))) {
    if (any(grepl(featsub[[featImporter]][i], consign_prod_div[[1]], 
                  ignore.case = TRUE))) {
      output <- c(output, 
                  mean(consign_prod_div[grep(
                    featsub[[featImporter]][i], consign_prod_div[[1]], 
                    ignore.case = TRUE), ][[2]]))
    } else {
      output <- c(output, 0)
    }
  }
  featsub$`product diversity` <- output
  
  return(featsub)
}

