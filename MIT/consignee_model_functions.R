## Distance Consignee to US Port ----
# Functions: getLatLong, getGCconsign, getGCusport, getDistConsignPort
# For every shipment in shipdf, get the distance from consignee city to US port.

# Get geocodes for every consignee address in shipdf.
getGCconsign <- function(shipdf, fdaConCity) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConCity = col index of consignee cities within shipdf.
  # Output is 2 col df listing every consignee city and its geocode.
  
  uniaddress <- unique(shipdf[[fdaConCity]][!is.na(shipdf[[fdaConCity]])])
  
  # Use method "dsk" to get geocode for every unique value in address.
  gc <- geocode(uniaddress, source = "dsk", messaging = FALSE)
  consignGC <- cbind(gc, uniaddress)
  consignGC$uniaddress <- as.character(consignGC$uniaddress)
  consignGC$lon <- unlist(consignGC$lon)
  consignGC$lat <- unlist(consignGC$lat)
  
  # For all NA's returned during method "dsk", use google maps API to try to 
  # fill in those gaps.
  ids <- unique(c(which(is.na(consignGC$lon)), which(is.na(consignGC$lat))))
  if (length(ids) > 0) {
    goog <- sapply(ids, function(x) 
      geocode(consignGC[x, ]$uniaddress, source = "google", messaging = FALSE))
    if (length(goog) > 0 && length(goog) == (length(ids) * 2)) {
      consignGC[ids, ]$lon <- goog[seq(1, length(goog), 2)]
      consignGC[ids, ]$lat <- goog[seq(2, length(goog), 2)]
    }
  }
  
  return(consignGC)
}

# Get geocodes for every consignee address in shipdf.
getGCusport <- function(shipdf, fdaUSport) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConCity = col index of consignee cities within shipdf.
  # Output is 2 col df listing every consignee city and its geocode.
  
  uniaddress <- unique(shipdf[[fdaUSport]][!is.na(shipdf[[fdaUSport]])])
  
  # Use method "dsk" to get geocode for every unique value in address.
  gc <- geocode(uniaddress, source = "dsk", messaging = FALSE)
  usportGC <- cbind(gc, uniaddress)
  usportGC$uniaddress <- as.character(usportGC$uniaddress)
  usportGC$lon <- unlist(usportGC$lon)
  usportGC$lat <- unlist(usportGC$lat)
  
  # For all NA's returned during method "dsk", use google maps API to try to 
  # fill in those gaps.
  ids <- unique(c(which(is.na(usportGC$lon)), which(is.na(usportGC$lat))))
  if (length(ids) > 0) {
    goog <- sapply(ids, function(x) 
      geocode(usportGC[x, ]$uniaddress, source = "google", messaging = FALSE))
    if (length(goog) > 0 && length(goog) == (length(ids) * 2)) {
      usportGC[ids, ]$lon <- goog[seq(1, length(goog), 2)]
      usportGC[ids, ]$lat <- goog[seq(2, length(goog), 2)]
    }
  }
  
  return(usportGC)
}

# Compile geocodes for every Place of Receipt in shipdf.
getGCpor <- function(shipdf, fdaPOR) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaUSport = col index of US ports within shipdf.
  # Output is 2 col df listing every US port and its geocode.
  print("'Now computing geocodes for all Places of Receipt...")
  
  uniPOR <- unique(shipdf[[fdaPOR]][shipdf[[fdaPOR]] != "unmatched"])
  uniPOR <- uniPOR[!is.na(uniPOR)]
  
  # Use method "dsk" to lookup geocodes.
  gc <- geocode(uniPOR, source = "dsk", messaging = FALSE)
  porGC <- cbind(gc, uniPOR)
  porGC$uniPOR <- as.character(porGC$uniPOR)
  porGC$lon <- unlist(porGC$lon)
  porGC$lat <- unlist(porGC$lat)
  
  # For all NA's returned during method "dsk", use google maps API to try to 
  # fill in those gaps.
  ids <- unique(c(which(is.na(porGC$lon)), which(is.na(porGC$lat))))
  if (length(ids) > 0) {
    goog <- sapply(ids, function(x) 
      geocode(porGC[x, ]$uniPOR, source = "google", messaging = FALSE))
    if (length(goog) > 0 && length(goog) == (length(ids) * 2)) {
      porGC[ids, ]$lon <- goog[seq(1, length(goog), 2)]
      porGC[ids, ]$lat <- goog[seq(2, length(goog), 2)]
    }
  }
  
  return(porGC)
}

# Compile geocodes for every Foreign Port in shipdf.
getGCfp <- function(shipdf, fdaFP) {
  # Params:
  # 
  # Output is
  print("Now computing geocodes for all Foreign Ports...")
  
  uniFP <- unique(shipdf[[fdaFP]][shipdf[[fdaFP]] != "unmatched"])
  uniFP <- uniFP[!is.na(uniFP)]
  
  # Use method "dsk" to lookup geocodes.
  gc <- geocode(uniFP, source = "dsk", messaging = FALSE)
  fpGC <- cbind(gc, uniFP)
  fpGC$uniFP <- as.character(fpGC$uniFP)
  fpGC$lon <- unlist(fpGC$lon)
  fpGC$lat <- unlist(fpGC$lat)
  
  # For all NA's returned during method "dsk", use google maps API to try to 
  # fill in those gaps.
  ids <- unique(c(which(is.na(fpGC$lon)), which(is.na(fpGC$lat))))
  if (length(ids) > 0) {
    goog <- sapply(ids, function(x) 
      geocode(fpGC[x, ]$uniFP, source = "google", messaging = FALSE))
    if (length(goog) > 0 && length(goog) == (length(ids) * 2)) {
      fpGC[ids, ]$lon <- goog[seq(1, length(goog), 2)]
      fpGC[ids, ]$lat <- goog[seq(2, length(goog), 2)]
    }
  }
  return(fpGC)
}

# Get country for every Place of Receipt in porGC.
getPorCountry = function(shipdf, porGC) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaPorGC = col index of Place of Receipt geocodes within shipdf.
  # Output is shipdf with 1 new column ("porCountry")
  print("Now computing country for every Place of Receipt...")
  
  L = c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
        "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", 
        "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", 
        "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", 
        "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Myanmar", "Burundi", 
        "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Republic", 
        "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", 
        "Côte d Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", 
        "Djibouti", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", 
        "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", 
        "France", "Faroe Islands", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", 
        "Greece", "Grenada", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", 
        "Honduras", "Hungary", "Hong Kong", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", 
        "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", 
        "Kenya","Kiribati","Korea","North Korea","South Korea","Kuwait","Kyrgyzstan", 
        "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", 
        "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", 
        "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", 
        "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", 
        "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", 
        "Nicaragua", "Niger", "Nigeria", "Niue", "North Korea", "Norway", "Oman", 
        "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", 
        "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "Romania", "Russia", 
        "Rwanda", "Samoa", "San Marino", "São Tomé and Príncipe", "Saudi Arabia", "Senegal", 
        "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", 
        "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", 
        "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", 
        "Syria", "Taiwan", "Tajikistan", "Tanzania", "Tatarstan", "Thailand", "Tibet", 
        "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
        "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
        "United States", "USA", "Uruguay", "Uzbekista", "Vanuatu", "Vatican", "Venezuela", 
        "Vietnam", "Yemen", "Zaire", "Zambia", "Zimbabwe")
  
  porGC$porCountry <- mapply(function(x, y) reverseGCwrapper(x, y, L), 
                             porGC$lon, porGC$lat, 
                             USE.NAMES = FALSE)
  
  return(porGC)
}

# Get country for every Foreign Port in fpGC.
getFpCountry = function(shipdf, fpGC) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaPorGC = col index of Place of Receipt geocodes within shipdf.
  # Output is shipdf with 1 new column ("porCountry")
  print("Now computing country for every Foreign Port...")
  
  L = c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
        "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", 
        "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", 
        "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", 
        "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Myanmar", "Burundi", 
        "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Republic", 
        "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", 
        "Côte d Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", 
        "Djibouti", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", 
        "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", 
        "France", "Faroe Islands", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", 
        "Greece", "Grenada", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", 
        "Honduras", "Hungary", "Hong Kong", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", 
        "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", 
        "Kenya","Kiribati","Korea","North Korea","South Korea","Kuwait","Kyrgyzstan", 
        "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", 
        "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", 
        "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", 
        "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", 
        "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", 
        "Nicaragua", "Niger", "Nigeria", "Niue", "North Korea", "Norway", "Oman", 
        "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", 
        "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "Romania", "Russia", 
        "Rwanda", "Samoa", "San Marino", "São Tomé and Príncipe", "Saudi Arabia", "Senegal", 
        "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", 
        "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", 
        "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", 
        "Syria", "Taiwan", "Tajikistan", "Tanzania", "Tatarstan", "Thailand", "Tibet", 
        "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
        "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
        "United States", "USA", "Uruguay", "Uzbekista", "Vanuatu", "Vatican", "Venezuela", 
        "Vietnam", "Yemen", "Zaire", "Zambia", "Zimbabwe")
  
  fpGC$fpCountry <- mapply(function(x, y) reverseGCwrapper(x, y, L), 
                           fpGC$lon, fpGC$lat, 
                           USE.NAMES = FALSE)
  
  return(fpGC)
}

reverseGCwrapper <- function(lon, lat, L) {
  if (any(is.na(c(lon, lat)))) {
    return(NA)
  }
  x <- reverseGeoCode(c(lat, lon))
  if (any(grepl("exceeded", x))) {
    warning(paste0("loop failed at iteration ", i, "."))
    warning(x)
    break
  }
  if (is.na(x)) {
    return(NA)
  }
  country <- NA
  for (j in seq_len(length(L))) {
    if (grepl(L[j], x, fixed = TRUE)) {
      country <- L[j]
      break
    }
  }
  return(country)
}

reverseGeoCode <- function(vect) {
  # Params:
  # latlng = character vector, latitude / longitude.
  # Output is the address/location that corresponds to the input lat/long.
  # NOTE: function uses Google Maps API, daily query limit (without a key) is 2500.
  latlngStr <-  gsub(' ','%20', paste(vect, collapse=","))
  connectStr <- 
    paste0('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',latlngStr)
  con <- url(connectStr)
  on.exit(close(con))
  rl <- tryCatch(readLines(con), error=function(e) e, warning=function(w) w)
  if (is(rl, "warning")) {
    address <- list(c(NA, NA))
  } else {
    data.json <- unlist(fromJSON(paste(rl, collapse="")))
    if (data.json["status"] == "OK") {
      address <- unname(data.json["results.formatted_address"])
    } else if (data.json["status"] == "OVER_QUERY_LIMIT") {
      address <- data.json["error_message"]
    } else if (data.json["status"] == "ZERO_RESULTS") {
      address <- NA
    }
  }
  Sys.sleep(0.2)
  return (address)  
}

# For each row in shipdf, calculate distance between the US Port and 
# Consignee city.
getDistConsignUSport <- function(consignAdd, usport, consignGC, usportGC) {
  # Params:
  # 
  # Output is
  if (is.na(consignAdd) || is.na(usport)) {
    return(NA)
  }
  usport_gc <- unname(unlist(usportGC[match(usport, usportGC$uniaddress), c(1, 2)]))
  consign_gc <- unname(unlist(consignGC[match(consignAdd, consignGC$uniaddress), c(1, 2)]))
  if (any(is.na(usport_gc) || any(is.na(consign_gc)))) {
    return(NA)
  }
  
  distance <- as.vector(distm(usport_gc, consign_gc, fun = distHaversine)/ 1000)
  return(distance)
}


## Get Counts: num manufacturers, num importers ----
# For each consignee, this function will get: total number of manufacturers 
# they work with, and number of importers they work with.
getCounts <- function(shipsub, fdaManu, fdaConsign, fdaImporter, featsub, 
                      featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipsub.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaImporter = col index of importer names within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 2 new columns ("numManufacturers", "numImporters")
  print("Now computing getCounts (num mfq & num importers...")
  
  featsub$numManufacturers <- NA
  featsub$numImporters <- NA
  for (i in featsub[[featConsign]]) {
    manu <- length(unique(shipsub[shipsub[[fdaConsign]] == 
                                      i, ][[fdaManu]]))
    featsub[featsub[[featConsign]] == i, ]$numManufacturers <- manu
    
    import <- length(unique(shipsub[shipsub[[fdaConsign]] == 
                                      i, ][[fdaImporter]]))
    featsub[featsub[[featConsign]] == i, ]$numImporters <- import
  }
  return(featsub)
}


## Fraction of weight coming from mfq that are also shippers ----
# For each consignee within featsub, get the fraction of total shipment weight
# that comes from shipments in which the manufacturer is also acting as the
# shipper.
kgFromManuShipperMatches <- function(shipsub, fdaConsign, fdaManu, fdaShipper, 
                                     featsub, featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaManu = col index of manufacturer names within shipsub.
  # fdaShipper = col index of shipper names within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column 
  # ("frac fo KG shipped when manu equals shipper")
  
  print("Now computing fraction of weight coming from mfq that are also shippers...")
  featsub$`frac of KG shipped when manu equals shipper` <- NA
  # Create list of lists that contain, for each consignee within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featConsign]], function(x) which(shipsub[[fdaConsign]] %in% x))
  # Get vector of the total weight shipped by each consignee within featsub.
  weights <- unname(sapply(ids, function(x) sum(shipsub[unlist(x), ][[fdaKG]])))
  # Get vector of the total weight shipped by each consignee within featsub but 
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
# For each consignee within featsub, get the mean (over the years) of the 
# consignees shipment weights normalized by the average weight of all 
# shipments during each year.
getNormWeight <- function(shipsub, fdaDate, fdaKG, featsub, featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaDate = col index of dates within shipsub.
  # fdaKG = col index of weight in KG within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
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
  for (i in featsub[[featConsign]]) {
    id <- which(shipsub[[fdaConsign]] %in% i)
    x <- mean(normweight[id], na.rm = TRUE)
    if (!is.na(x) && x < 200) {
      featsub[featsub[[featConsign]] == i, ]$normalizedWeight <- x
    } else {
      featsub[featsub[[featConsign]] == i, ]$normalizedWeight <- NaN
    }
  }
  return(featsub)
}


## Number of Unique US Ports ----
# For each consignee within featsub, get count of unique US ports from shipsub.
getUSPorts <- function(shipsub, fdaConsign, fdaUSport, featsub, featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaUSport = col index of US ports within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("numUSports")
  print("Now computing number of US ports...")
  featsub$numUSports <- unname(
    sapply(featsub[[featConsign]], function(x) 
      length(
        unique(shipsub[which(shipsub[[fdaConsign]] %in% x), ][[fdaUSport]]))))
  
  return(featsub)
}


## Total weight below 10% quantile ----
# For each consignee within featsub, this is a binary variable that indicates 
# if the total weight received by the consignee falls in the bottom 10% of 
# all total weights received by all consignees.
getExperience <- function(shipsub, fdaConsign, fdaKG, featsub, featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaKG = col index of weight in KG within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("consignee inexperienced")
  print("Now computing total weight below 10% quantile...")
  # Create list of lists that contain, for each consignee within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featConsign]], function(x) which(shipsub[[fdaConsign]] %in% x))
  # Get vector of the total weight shipped by each consignee within featsub.
  weights <- unname(sapply(ids, function(x) sum(shipsub[unlist(x), ][[fdaKG]])))
  # Create binary vector indicating, for each consignee, whether their total 
  # weight shipped is in the bottom 10 percentile of all consignees. Append this 
  # vector to featsub.
  featsub$`consignee inexperienced` <- unname(
    ifelse(weights > quantile(weights, 0.10, na.rm = TRUE), 0, 1))
  
  return(featsub)
}


## Avg Distances ----
# For each consignee within featsub, this calculates:
# 1. Mean distance between mfq and place of receipt across the all shipments.
# 2. Mean distance between place of receipt and foreign port across all shipments.
# 3. Fraction of transnational shipments between mfg and place of receipt.
# 4. Fraction of transnational shipments between place of receipt and foreign port.
getAvgDistances <- function(shipsub, fdaManu, fdaConsign, fdaCountry, 
                            fdaPorCountry, fdaFPcountry, featsub, featConsign, 
                            featdf, featManu, featDist1, featDist2) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipsub.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaCountry = col index of manufacturer countries within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
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
        any(is.na(shipsub[[fdaPOR]][[i]]))) {
      trans_manu_por <- c(trans_manu_por, NA)
    } else {
      porctry <- porGC[which(porGC$uniPOR == shipsub[[fdaPOR]][[i]]), ]$porCountry
      trans_manu_por <- c(trans_manu_por, 
                          ifelse(shipsub[[fdaCountry]][[i]] == porctry, 0, 1))
    }
  }
  # Create binary vector indicating, for each row of shipsub, whether 
  # Place of Receipt country == Foreign Port country
  trans_fp_por <- vector()
  for (i in seq_len(nrow(shipsub))) {
    if (any(is.na(shipsub[[fdaFP]][[i]])) || 
        any(is.na(shipsub[[fdaPOR]][[i]]))) {
      trans_fp_por <- c(trans_fp_por, NA)
    } else {
      porctry <- porGC[which(porGC$uniPOR == shipsub[[fdaPOR]][[i]]), ]$porCountry
      fpctry <- fpGC[which(fpGC$uniFP == shipsub[[fdaFP]][[i]]), ]$fpCountry
      trans_fp_por <- c(trans_fp_por, ifelse(porctry == fpctry, 0, 1))
    }
  }
  
  for (i in seq_len(nrow(featsub))) {
    id <- which(shipsub[[fdaConsign]] %in% featsub[[featConsign]][i])
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



## Avg Distance Consignee City to US Port ----
# For each consignee within featsub, get the mean distance between the consignee
# and the US port for each of their shipments.
getAvgDistConsignUSport <- function(shipsub, fdaConsign, fdaKG, featsub, 
                                    featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaKG = col index of weights in KG within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("distance consignee - US port")
  print("Now computing avg distance consignee city to US port...")
  featsub$`distance consignee - US port` <- NA
  for (i in featsub[[featConsign]]) {
    id <- which(shipsub[[fdaConsign]] %in% i)
    featsub[featsub[[featConsign]] == i, ]$`distance consignee - US port` <- 
      log10(mean(shipsub[id, ][[fdaKG]], na.rm = TRUE))
  }
  
  # Rewrite all results of "-Inf" to be "0".
  if (any(is.infinite(featsub$`distance consignee - US port`))) {
    featsub[is.infinite(
      featsub$`distance consignee - US port`), ]$`distance consignee - US port` <- 0
  }
  return(featsub)
}


## Variation of Num of Shipments and Num of Manufacturers Across Years ----
# For each consignee in shipsub, get the variation of number of shipments 
# across all years in which that consignee was active.
getVariations <- function(shipsub, fdaDate, fdaConsign, featManu, featsub, 
                          featConsign) {
  # Params:
  # shipsub = dataframe, FDA shipping data.
  # fdaDate = col index of the ship date within shipsub.
  # fdaConsign = col index of the conignee names within shipsub.
  # fdaManu = col index of the manufacturer names within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("shipment variation")
  print("Now computing variation of num shipments & num manufacturers...")
  
  # Start by getting all years in which the consignee was active
  # i.e. had shipments, and the number of shipments in each of those years.
  featsub$`shipment variation` <- NA
  featsub$`manufacturer variation` <- NA
  shipsub$year <- as.numeric(format(shipsub[[fdaDate]], "%Y"))
  for (i in featsub[[featConsign]]) {
    yr <- unique(shipsub[shipsub[[fdaConsign]] == i, ]$year)
    nship <- unname(
      sapply(yr, function(x) 
        nrow(shipsub[shipsub[[fdaConsign]] == i & shipsub$year == x, ])))
    nmanu <- unname(
      sapply(yr, function(x) 
        length(unique(shipsub[shipsub[[fdaConsign]] == i & 
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
      featsub[featsub[[featConsign]] == i, ]$`shipment variation` <- 0
      featsub[featsub[[featConsign]] == i, ]$`manufacturer variation` <- 0
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
      featsub[featsub[[featConsign]] == i, ]$`shipment variation` <- SD / avgShip
      SD <- sqrt(sum(numeratorManu) / (yrfull[length(yrfull)] - yrfull[1]) + 10e-6)
      featsub[featsub[[featConsign]] == i, ]$`manufacturer variation` <- SD / avgManu
    }
  }
  return(featsub)
}


## Dispersion of Total Shipment Weight per Manufacturer ----
# For each consignee, get the dispersion of the total shipment weight per 
# manufacturer that the consignee has worked with.
# This dispersion calculation is a measure of entropy.
getManuDispersion <- function(shipsub, fdaConsign, fdaManu, fdaKG, 
                                   featsub, featConsign) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConsign = col index of the consignee names within shipdf.
  # fdaManu = col index of the manufacturer names within shipdf.
  # fdaKG = col index of weights in KG within shipdf.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("manufacturer dispersion")
  print("Now computing dispersion of total shipment weight per mfq...")
  
  featsub$`manufacturer dispersion` <- NA
  for (i in seq_len(nrow(featsub))) {
    if (featsub$numManufacturers[i] == 0) {
      featsub$`manufacturer dispersion`[i] <- 0
      next
    }
    df <- shipsub[shipsub[[fdaConsign]] == featsub[[featConsign]][i] & 
                        !is.na(shipsub[[fdaConsign]]), c(fdaConsign, fdaManu, fdaKG)]
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
# For each consignee within featsub, get the fraction of their shipments
# that came from a manufacturer in China.
getChineseShipments <- function(shipsub, fdaCountry, fdaConsign, featsub, 
                                featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaCountry = col index of manufacturer countries within shipsub.
  # fdaConsign = col index of consignee names within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("chineseShipments").
  print("Now computing fraction of shipments from china...")
  
  # Create list of lists that contains, for each consignee within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featConsign]], function(x) which(shipsub[[fdaConsign]] %in% x))
  # Get vector of the total num of shipments by each consignee within featsub.
  nship <- unname(sapply(ids, function(x) length(unlist(x))))
  # Get vector of num of Chinese shipments for each consignee within featsub.
  chinese <- unname(
    sapply(ids, function(x) 
      sum(shipsub[[fdaCountry]][unlist(x)] == "China")))
  featsub$chineseShipments <- chinese / nship
  return(featsub)
}


## Number of Sampled Shipments ----
# For each consignee within featsub, get the log of the total number of 
# sampled shipments.
getSampled <- function(shipsub, fdaConsign, fdaSamp, featsub, featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaSamp = col index of sampling info within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("numSampled")
  print("Now computing number of sampled shipments...")
  
  # Create list of lists that contains, for each consignee within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featConsign]], function(x) which(shipsub[[fdaConsign]] %in% x))
  
  binarySamp <- ifelse(!is.na(shipsub[[fdaSamp]]), 1, 0)
  numSamp <- unname(sapply(ids, function(x) sum(binarySamp[unlist(x)])))
  featsub$numSampled <- log10(1 + numSamp)
  return(featsub)
}


## Number of Refused Shipments ----
# For each consignee within featsub, get the log of the total number of 
# refused shipments.
getRefused <- function(shipsub, fdaConsign, fdaRefuse, featsub, featConsign) {
  # Params:
  # shipsub = dataframe, subset of the FDA shipping data.
  # fdaConsign = col index of consignee names within shipsub.
  # fdaRefuse = col index of refusal info within shipsub.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 1 new column ("numRefused").
  print("Now computing number of refused shipments...")
  
  # Create list of lists that contains, for each consignee within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featConsign]], function(x) which(shipsub[[fdaConsign]] %in% x))
  
  binaryRef <- grepl("154|155", shipsub[[fdaRefuse]])
  numRef <- unname(sapply(ids, function(x) sum(binaryRef[unlist(x)])))
  featsub$numRefused <- log10(1 + numRef)
  
  return(featsub)
}


## Binary Inspection Variables ----
# For each consignee within featsub, record whether or not they: 
# 1. Were inspected prior to year p.
# 2. Were inspected in year p.
# 3. Had a bad insp in year p (VAI or OAI or RTS).
getBinaryInspVars <- function(insp, inspDate, inspResult, featsub, 
                                featConsign, p) {
  # Params:
  # insp = dataframe, FDA site inspection data.
  # inspDate = col index of inspection dates within insp.
  # inspResult = col index of inspection outcomes within insp.
  # featsub = features dataframe.
  # featConsign = col index of consignee names within featsub.
  # Output is featsub with 3 new columns ("insp prior to p", "insp in p", 
  # "bad insp in p")
  print("Now computing binary inspection variables...")
  
  insp$year <- as.numeric(format(insp[[inspDate]], "%Y"))
  # Create list of lists that contains, for each consignee within featsub, their 
  # indices within shipsub.
  ids <- lapply(
    featsub[[featConsign]], function(x) which(insp[[inspName]] %in% x))
  
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
# functions = product_diversity, getProductDiv
product_diversity <- function(productIG, sparsity=0.95) {
  # Params: 
  # productIG = dataframe, scraped shipping data from IG
  # Output is dataframe of consignee names with their product diversity score.
  print("Now computing product diversity...")
  
  # Step 1: Create corpus object from the products list.
  corpus = Corpus(VectorSource(productIG$product))
  
  # Step 2: Change all the text to lower case.
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  
  # Step 3: Remove all punctuation.
  corpus = tm_map(corpus, removePunctuation)
  
  # Step 4: Remove stop words.  
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  # Step 5: Stem document 
  corpus = tm_map(corpus, stemDocument)
  
  # Step 6: Create a word count matrix (rows are shipments, columns are words)
  frequencies = DocumentTermMatrix(corpus)
  
  # Step 7: Account for sparsity.
  # keep terms that appear in 5% or more of the product descriptions
  #sparse = removeSparseTerms(frequencies, sparsity)
  
  # Step 8: Create data frame from the document-term matrix
  #allproductTM = as.matrix(sparse)
  allproductTM = as.matrix(frequencies)
  allproductTM[allproductTM > 1] <- 1
  allproductTM = cbind(consignee=productIG$consignee, allproductTM) 
  allproductTM = as.data.frame(allproductTM, stringsAsFactors=FALSE)
  allproductTM[, 2:ncol(allproductTM)] <- sapply(allproductTM[, 2:ncol(allproductTM)], 
                                                 as.numeric)
  product_count = as.matrix(allproductTM %>% 
                              group_by(consignee) %>% 
                              summarise_each(funs(sum)))
  product_count = product_count[,2:ncol(product_count)]
  class(product_count) <- "numeric" 
  product_count[product_count > 1] <- 1
  products <- t(product_count)%*%product_count
  diag(products) <- 0
  
  #Choose specific products
  products <- products[c(1:10),c(1:10)]
  g <- graph.adjacency(products, mode="undirected", weighted=TRUE)
  
  #Compute consignee's product diversity feature
  consignees=unique(productIG$consignee)
  productIG$product.name=rep(0,nrow(productIG))
  product_names <- toupper(colnames(products))
  
  for (i in 1:nrow(productIG)) {
    for (j in 1:length(product_names)){
      prod=product_names[j]
      if (grepl(as.character(prod),as.character(productIG$product[i]))){
        productIG$product.name[i]=as.character(prod)
        break
      }
    }
  }
  
  productIG <- subset(productIG,productIG[,4]!=0)
  d <- data.frame(consignees, product_diversity=rep(0,length(consignees)), 
                  stringsAsFactors = FALSE)
  
  for (i in 1:length(consignees)) {
    consignee_products <- unique(productIG[productIG[[1]] == 
                                             consignees[i], ][[4]])
    mship <- rep(FALSE, length(product_names))
    if (length(consignee_products) > 1) {
      for (j in 1:length(consignee_products)) {
        for (k in 1:length(product_names)) {
          if (product_names[k] == consignee_products[j]) {
            mship[k] <- TRUE
          }
        }
      }
      d[i,2] <- -(1/(4*length(E(g))))*t(membership) %*% 
        modularity_matrix(g,membership) %*% 
        membership
    }
  }
  
  return(d)
}

getProductDiv <- function(featsub, featConsign, productIG) {
  # Calculate the product diversity for each unique consignee within the tidy
  # IG dataset (output is a 2 col df: firm name, prod div score).
  prod_div <- product_diversity(productIG)
  
  # Create vector of indices, locations of each consignee from featsub within
  # aminesdf.
  #id <- unname(sapply(featsub[[featConsign]], function(x) agrep(x, aminesdf[[1]], ignore.case = TRUE)))
  output <- vector()
  for (i in seq_len(nrow(featsub))) {
    if (any(agrepl(featsub[[featConsign]][i], prod_div[[1]], ignore.case = TRUE))) {
      x <- prod_div[agrep(featsub[[featConsign]][i], prod_div[[1]], ignore.case = TRUE), ]
      output <- c(output, x[which.max(x[[2]]), ][[2]])
    } else {
      output <- c(output, 0)
    }
  }
  featsub$`product diversity` <- output
  
  return(featsub)
}


