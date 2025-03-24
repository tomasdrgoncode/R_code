## Get matches between FDA data and IG data ----
## Match observations within the FDA commodity data with those of the IG data.
# Functions: igPrep, commodityPrep, getMatches

igPrep <- function(df, igDate) {
  # Params:
  # df = dataframe, the scraped IG data.
  # igDate = col index of the arrival dates witin IG dataset.
  # Output is the IG df with transformations applied.
  df[[igDate]] <- as.Date(df[[igDate]], format = "%m/%d/%Y")
  df <- df[order(df[[igDate]]), ]
  return(df)
}

commodityPrep <- function(df, fdaManu, fdaDate, fdaCountry, fdaShipType) {
  # Params:
  # df = dataframe, the FDA shipping data.
  # fdaManu = col index of manufacturer names within df.
  # fdaDate = col index of dates within df.
  # fdaCountry = col index of manufacturer countries within df.
  # fdaShipType = col index of shpiment types within df.
  # Output is the fda df with transformations applied.
  df <- df[grepl("Sea", df[[fdaShipType]]), ]
  if (any(grepl("Taiwan|Hong Kong", df[[fdaCountry]]))) {
    df[grepl("Taiwan|Hong Kong", df[[fdaCountry]]), ][[fdaCountry]] <- "China"
  }
  df <- df[df[[fdaManu]] != "Not Supplied", ]
  df[[fdaDate]] <- as.Date(df[[fdaDate]], format = "%m/%d/%Y")
  df <- df[order(df[[fdaDate]]), ]
  df$`Place of Receipt` <- "unmatched"
  df$`Foreign Port` <- "unmatched"
  return(df)
}

getMatches <- function(fdadf, fdaManu, fdaDate, fdaBill, fdaContain, fdaCarr, 
                       fdaVoy, fdaCountry, fdaShipType,  IGdf, igDate, igBill, 
                       igContain, igCarr, igVoy, igPOR, igForeign) {
  # Params:
  # fdadf = dataframe, the FDA shipping data.
  # IGdf = dataframe, the scraped IG data.
  # fdaManu = col index of manufacturer names within fdadf.
  # fdaDate / igDate = col indices of dates within both fdadf & IGdf.
  # fdaBill / igBill = col indices of Bill of Lading within both df's.
  # fdaContain / igContain = col indices of Container Number within both df's.
  # fdaCarr / igCarr = col indices of Carrier Code within both df's.
  # fdaVoy / igVoy = col indices of Voyage Number within both df's.
  # fdaCountry = col index of manufacturer countries within fdadf.
  # fdaShipType = col index of shpiment types within df.
  # igPOR = col index of Place of Receipt within IGdf.
  # igForeign = col index of Foreign Port within IGdf.
  # Output is fdadf with 2 new columns ("Place of Receipt" and "Foreign Port")
  print("Now computing FDA shipping / IG Matching...")
  
  
  # Data prep
  fdadf <- commodityPrep(fdadf, fdaManu, fdaDate, fdaCountry, fdaShipType)
  IGdf <- igPrep(IGdf, igDate)
  
  # Create variables
  IGbill <- IGdf[[igBill]]
  IGcontain <- IGdf[[igContain]]
  IGcarr <- IGdf[[igCarr]]
  IGvoy <- IGdf[[igVoy]]
  IGpor <- IGdf[[igPOR]]
  IGforeign <- IGdf[[igForeign]]
  IGdate <- IGdf[[igDate]]
  FDAncol <- ncol(fdadf)
  
  # Start the for loop
  for (i in seq_len(nrow(fdadf))) {
    shrimpdate <- fdadf[[fdaDate]][[i]]
    if (is.na(shrimpdate)) {
      next
    }
    shrimpbill <- fdadf[[fdaBill]][[i]]
    shrimpcontain <- fdadf[[fdaContain]][[i]]
    shrimpcarr <- fdadf[[fdaCarr]][[i]]
    shrimpvoy <- fdadf[[fdaVoy]][[i]]
    if (i > 1 && 
        !is.na(fdadf[[fdaDate]][[i-1]]) && 
        !is.na(fdadf[[fdaBill]][[i-1]]) && 
        !is.na(shrimpbill) && 
        shrimpdate == fdadf[[fdaDate]][[i-1]] && 
        substr(as.character(shrimpbill), 1, 10) == 
        substr(as.character(fdadf[[fdaBill]][[i-1]]), 1, 10)) {
      fdadf[[FDAncol - 1]][[i]] <-  fdadf[[FDAncol - 1]][[i-1]]
      fdadf[[FDAncol]][[i]] <- fdadf[[FDAncol]][[i-1]]
      next
    } else if (i > 1 && 
               !is.na(fdadf[[fdaDate]][[i-1]]) && 
               !is.na(fdadf[[fdaContain]][[i-1]]) && 
               !is.na(shrimpcontain) && 
               shrimpdate == fdadf[[fdaDate]][[i-1]] && 
               shrimpcontain == fdadf[[fdaContain]][[i-1]]) {
      fdadf[[FDAncol - 1]][[i]] <- fdadf[[FDAncol - 1]][[i-1]]
      fdadf[[FDAncol]][[i]] <- fdadf[[FDAncol]][[i-1]]
      next
    } else if (i > 1 && 
               !is.na(fdadf[[fdaDate]][[i-1]]) && 
               !is.na(fdadf[[fdaCarr]][[i-1]]) && 
               !is.na(shrimpcarr) && 
               !is.na(fdadf[[fdaVoy]][[i-1]]) && 
               !is.na(shrimpvoy) && 
               shrimpdate == fdadf[[fdaDate]][[i-1]] && 
               shrimpcarr == fdadf[[fdaCarr]][[i-1]] && 
               shrimpvoy == fdadf[[fdaVoy]][[i-1]]) {
      fdadf[[FDAncol - 1]][[i]] <- fdadf[[FDAncol - 1]][[i-1]]
      fdadf[[FDAncol]][[i]] <- fdadf[[FDAncol]][[i-1]]
      next
    }
    if (!is.na(shrimpbill) && any(grepl(shrimpbill, IGbill, fixed = TRUE))) {
      id <- grep(shrimpbill, IGbill, fixed = TRUE)[1]
      if (IGdate[id] >= (shrimpdate - 10) && 
          IGdate[id] <= (shrimpdate + 10)) {
        fdadf[[FDAncol - 1]][[i]] <- IGpor[id]
        fdadf[[FDAncol]][[i]] <- IGforeign[id]
      }
    } else if (!is.na(shrimpcontain) && any(grepl(shrimpcontain, IGcontain, fixed = TRUE))) {
      id <- grep(shrimpcontain, IGcontain, fixed = TRUE)[1]
      if (IGdate[id] >= (shrimpdate - 10) && 
          IGdate[id] <= (shrimpdate + 10)) {
        fdadf[[FDAncol - 1]][[i]] <- IGpor[id]
        fdadf[[FDAncol]][[i]] <- IGforeign[id]
      }
    } else if (!is.na(shrimpcarr) && !is.na(shrimpvoy) && 
               any(grep(shrimpcarr, IGcarr, fixed = TRUE) == 
                   which(IGvoy == shrimpvoy))) {
      id <- intersect(grep(shrimpcarr, IGcarr, fixed = TRUE), 
                      which(IGvoy == shrimpvoy))[1]
      if (IGdate[id] >= (shrimpdate - 10) && 
          IGdate[id] <= (shrimpdate + 10)) {
        fdadf[[FDAncol - 1]][[i]] <- IGpor[id]
        fdadf[[FDAncol]][[i]] <- IGforeign[id]
      }
    }
  }
  return(fdadf)
}


## Get Counts: Shipments, num consignees, num shippers ----
# For each manufacturer, this function will get: total number of shipments, 
# number of consignees they work with, and number of shippers they work with.

getCounts <- function(shipdf, fdaManu, fdaConsign, fdaShipper, featdf, featManu) {
  # Params:
  # shipdf = dataframe, the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipdf.
  # fdaConsign = col index of consignee names within shipdf.
  # fdaShipper = col index of shipper names within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 3 new columns ("numShipments", "numConsignees", and 
  # "numShippers")
  print("Now computing Get Counts...")
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  featdf$numShipments <- sapply(ids, function(x) length(unlist(x)))
  featdf$numConsignees <- sapply(ids, function(x) 
    length(unique(shipdf[unlist(x), ][[fdaConsign]])))
  featdf$numShippers <- sapply(ids, function(x) 
    length(unique(shipdf[unlist(x), ][[fdaShipper]])))
  
  return(featdf)
}

## Normalized Yearly Average Weight ----
# Functions: getKilograms, getNormWeight

# Convert as many of the weights in shipdf to kilograms as possible.
getKilograms <- function(shipdf, fdaWeight, fdaUnit) {
  # Params:
  # shipdf = dataframe, the FDA shipping data.
  # fdaWeight = col index of weights within fdadf.
  # fdaUnit = col index of weight units within fdadf.
  # Output is df with one new column ("weightKG").
  univals <- unique(shipdf[[fdaUnit]])
  shipdf$weightKG <- NA
  # Kilograms
  if (any(grepl("^[Kk]ilograms", univals))) {
    id <- grep("^[Kk]ilograms", shipdf[[fdaUnit]])
    shipdf[id, ]$weightKG <- shipdf[id, ][[fdaWeight]]
  }
  # Ounces
  if (any(grepl("^[Oo]unces, [^Ff]", univals))) {
    id <- grep("^[Oo]unces, [^Ff]", shipdf[[fdaUnit]])
    shipdf[id, ]$weightKG <- shipdf[id, ][[fdaWeight]] / 35.274
  }
  # Pounds
  if (any(grepl("^[Pp]ounds", univals))) {
    id <- grep("^[Pp]ounds", shipdf[[fdaUnit]])
    shipdf[id, ]$weightKG <- shipdf[id, ][[fdaWeight]] / 2.2046
  }
  # Grams
  if (any(grepl("^[Gg]rams", univals))) {
    id <- grep("^[Gg]rams", shipdf[[fdaUnit]])
    shipdf[id, ]$weightKG <- shipdf[id, ][[fdaWeight]] / 1000
  }
  return(shipdf)
}

getNormWeight <- function(shipdf, fdaDate, fdaKG, fdaManu, featdf, featManu) {
  # Params:
  # shipdf = dataframe, the FDA shipping data.
  # fdaDate = col index of dates within shipdf.
  # fdaKG = col index of weight in KG within shipdf.
  # fdaManu = col index of manufacturer names within shipdf.
  # featdf = dataframe, features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("normalizedweight").
  print("Now computing Normalized Yearly Average Weight...")
  
  # Create table of each unique year found within shipdf, each row includes year
  # and mean of all weights (in KG) for that year.
  avgs <- vector()
  uniyears <- unique(as.numeric(format(shipdf[[fdaDate]], "%Y")))
  for (i in uniyears) {
    avgs <- c(avgs, 
              mean(
                shipdf[as.numeric(format(shipdf[[fdaDate]], "%Y")) == i, ][[fdaKG]], 
                na.rm = TRUE))
  }
  weightsdf <- data_frame(years=uniyears, mean=avgs)
  # For every observation in shipdf, take value weightKG and divide by the mean 
  # from weightsdf that corresponds with the year of the observation from shipdf,
  # save output in vector normweight.
  normweight <- vector()
  for (i in seq_len(nrow(shipdf))) {
    x <- weightsdf[weightsdf$years == 
                     as.numeric(format(shipdf[[fdaDate]][i], "%Y")), ]$mean
    normweight <- c(normweight, (shipdf[[fdaKG]][i] / x))
  }
  
  # For each manufacturer, get mean of all the values compiled within normweight, 
  # append the output to all of the manufacturer's observations within shipdf.
  featdf$normalizedweight <- NA
  x <- vector()
  for (i in seq_len(nrow(featdf))) {
    id <- which(shipdf[[fdaManu]] == featdf[[featManu]][i])
    x <- c(x, mean(normweight[id], na.rm = TRUE))
  }
  x[is.na(x) | x > 200] <- NaN
  featdf$normalizedweight <- x

  return(featdf)
}


## Manufacturer is also shipper ----
# For each manufacturer, this is the fraction of shipments in which the 
# shipper is also the manufacturer.
getManuShipperMatches <- function(shipdf, fdaManu, fdaShipper, featdf, featManu) {
  # Params:
  # shipdf = dataframe, the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipdf.
  # fdaShipper = col index of shipper names within shipdf.
  # featdf = dataframe, features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with one new column ("Manu is also Shipper").
  print("Now computing Manufacturer is also Shipper...")
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  shipISmanu <- ifelse(shipdf[[fdaShipper]] == shipdf[[fdaManu]], 1, 0)
  
  nShippers <- sapply(ids, function(x) sum(shipISmanu[unlist(x)]))
  featdf$`Manu is also Shipper` <- nShippers / featdf$numShipments
  
  return(featdf)
}


## Geocodes ----
# Compile vector of address given, city, province, and country.
getAddress <- function(city, prov, country) {
  # Params:
  # shipdf = dataframe, the FDA shipping data.
  # fdaCity = single numeric value, index of the city column.
  # fdaProv = single numeric value, index of the province column.
  # fdaCountry = single numeric value, index of the country column.
  # Output is a character vector of addresses.
  if (!is.na(country)) {
    if (!is.na(city) && !is.na(prov)) {
      address <- paste(city, prov, country, sep = ", ")
    } else if (!is.na(city) && is.na(prov)) {
      address <- paste(city, country, sep = ", ")
    } else if (is.na(city) && !is.na(prov)) {
      address <- paste(prov, country, sep = ", ")
    }
  } else {
    address <- NA
  }
  return(address)
}

# Compile manufacturer address for each observation in shipdf.
getFullAddress <- function(shipdf, fdaCity, fdaProv, fdaCountry) {
  # Params:
  # 
  # Output is
  
  # Compile vector of full addresses, one for each observation of shipdf.
  address <- mapply(getAddress, shipdf[[fdaCity]], shipdf[[fdaProv]], 
                    shipdf[[fdaCountry]], USE.NAMES = FALSE)
  
  # Rename South Korean addresses.
  address <- unname(
    sapply(address, function(x) 
      gsub("Korea (the Republic of)", "South Korea", x, fixed = TRUE)))
  
  shipdf$fulladdress <- address
  return(shipdf)
}

# Get geocodes for every manufacturer address in shipdf.
getGCmanu <- function(shipdf, fdaManuAddress) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConCity = col index of consignee cities within shipdf.
  # Output is 2 col df listing every consignee city and its geocode.
  print("Now computing geocodes for all manufacturers...")
  
  uniaddress <- unique(shipdf[[fdaManuAddress]][!is.na(shipdf[[fdaManuAddress]])])
  
  # Use method "dsk" to get geocode for every unique value in address.
  gc <- geocode(uniaddress, source = "dsk", messaging = FALSE)
  manuGC <- cbind(gc, uniaddress)
  manuGC$uniaddress <- as.character(manuGC$uniaddress)
  manuGC$lon <- unlist(manuGC$lon)
  manuGC$lat <- unlist(manuGC$lat)
  
  # For all NA's returned during method "dsk", use google maps API to try to 
  # fill in those gaps.
  ids <- unique(c(which(is.na(manuGC$lon)), which(is.na(manuGC$lat))))
  if (length(ids) > 0) {
    goog <- sapply(ids, function(x) 
      geocode(manuGC[x, ]$uniaddress, source = "google", messaging = FALSE))
    if (length(goog) > 0 && length(goog) == (length(ids) * 2)) {
      manuGC[ids, ]$lon <- goog[seq(1, length(goog), 2)]
      manuGC[ids, ]$lat <- goog[seq(2, length(goog), 2)]
    }
  }
  
  return(manuGC)
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


## Reverse Geocodes ----
# Convert geocode to an address/location.
reverseGeoCode <- function(latlng) {
  # Params:
  # latlng = character vector, latitude / longitude.
  # Output is the address/location that corresponds to the input lat/long.
  # NOTE: function uses Google Maps API, daily query limit (without a key) is 2500.
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))
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

## Distance manufacturer - place of receipt ----
# Functions: getAddresses, getLatLong, getGCmanu, getGCpor, getAvgDistManuPOR
# Average distance between the manufacturer and the place of receipt 
# across all the manufacturer's shipments.

# For each manufacturer within shipdf, get mean distance between manu and 
# all of their places of receipt, then get the log of that mean.
getDistManuPOR <- function(manuAdd, por, manuGC, porGC) {
  # Params:
  # 
  # Output is
  if (is.na(manuAdd) || is.na(por) || por == "unmatched") {
    return(NA)
  }
  por_gc <- unname(unlist(porGC[match(por, porGC$uniPOR), c(1, 2)]))
  manu_gc <- unname(unlist(manuGC[match(manuAdd, manuGC$uniaddress), c(1, 2)]))
  if (any(is.na(por_gc) || any(is.na(manu_gc)))) {
    return(NA)
  }
  
  distance <- as.vector(distm(por_gc, manu_gc, fun = distHaversine)/ 1000)
  return(distance)
}

# For each manufacturer within shipdf, get mean distance between manu and 
# all of their places of receipt, then get the log of that mean.
getAvgDistManuPOR <- function(shipdf, fdaManu, fdaManuAddress, fdaPOR, 
                              manuGC, porGC, featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaManu = col index of manufacturer names within shipdf.
  # fdaManuGC = col index of manuGeoCodes within shipdf.
  # fdaPorGC = col index of porGeoCodes within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("distance manufacturer - place of receipt").
  
  # Create vector that gives, for each element within shipdf, its index within 
  # either porGC$uniPOR or manuGC$uniaddress.
  #por_ids <- unname(sapply(shipdf[[fdaPOR]], function(x) match(x, porGC$uniPOR, incomparables = c("unmatched", NA))))
  #manu_ids <- unname(sapply(shipdf[[fdaManuAddress]], function(x) match(x, manuGC$uniaddress, incomparables = c("unmatched", NA))))
  
  # Calculate distances for each pair of manufacturer address geocodes and 
  # Place of Receipt geocodes.
  distanc <- mapply(function(x, y) 
    getDistManuPOR(x, y, manuGC, porGC), 
    shipdf[[fdaManuAddress]], shipdf[[fdaPOR]], USE.NAMES = FALSE)
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  # For each manufacturer, get mean of all of their values found witin 
  # vector "distanc", append the answer to each of that manufacturer's
  # observations within new column "distance manufacturer - place of receipt".
  y <- sapply(ids, function(x) log10(mean(distanc[unlist(x)], na.rm = TRUE)))
  
  # Rewrite all results of "-Inf" to be "0".
  if (any(is.infinite(y))) {
    y[is.infinite(y)] <- 0
  }
  
  featdf$`distance manufacturer - place of receipt` <- y
  
  return(featdf)
}


## Transnational manufacturer - place of receipt ----
# Functions: reverseGeocode, getPorCountry, getTransManuPor

# Country lookup function.
manu_por_countrylookup <- function(manuCountry, por, porGC) {
  # Params:
  # 
  # Output is
  if (is.na(manuCountry) || is.na(por) || por == "unmatched") {
    return(NA)
  }
  porCountry <- porGC[match(por, porGC$uniPOR), ]$porCountry
  if (is.na(porCountry)) {
    return(NA)
  }
  
  if (manuCountry == porCountry) {
    equal <- 0
  } else {
    equal <- 1
  }
  return(equal)
}

# For each manufacturer within featdf, get fraction of shipments
# where the manufacturer and the place of receipt are located in different countries
getTransManuPor <- function(shipdf, fdaManu, fdaCountry, fdaPOR, 
                            featdf, featManu, porGC) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # manuCountry = col index of manufacturer country within shipdf.
  # fdaCountry = col index of manufacturer countries within shipdf.
  # fdaPorCountry = col index of Place of Receipt country within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("transnational manufacturer - place of receipt")
  
  # Compile binary vector, each value corresponds to 1 row in shipdf, values are
  # zero if the manufacturer country and the Place of Recepit country within a single 
  # row are the same, and one if the two countries are different.
  
  # create binary vector, length == nrow(shipdf), each element indicates whether 
  # the manu country and por country for that row are equal or not.
  trans_manu_por <- mapply(function(x, y) manu_por_countrylookup(x, y, porGC), 
                           shipdf[[fdaCountry]], shipdf[[fdaPOR]], 
                           USE.NAMES = FALSE)
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  featdf$`transnational manufacturer - place of receipt` <- 
    sapply(ids, function(x) mean(trans_manu_por[unlist(x)], na.rm = TRUE))
  
  return(featdf)
}


## Distance Place of Receipt - Foreign Port ----
# Functions: getAddresses, getLatLong, getGCmanu, getGCpor, getAvgDistManuPOR
# Average distance between the manufacturer and the place of receipt 
# across all the manufacturer's shipments.

# For each manufacturer within shipdf, get mean distance between manu and 
# all of their places of receipt, then get the log of that mean.
getDistPorFp <- function(por, fp, porGC, fpGC) {
  # Params:
  # 
  # Output is
  if (any(is.na(c(por, fp))) || any(c(por, fp) == "unmatched")) {
    return(NA)
  }
  fp_gc <- unname(unlist(fpGC[match(fp, fpGC$uniFP), c(1, 2)]))
  por_gc <- unname(unlist(porGC[match(por, porGC$uniPOR), c(1, 2)]))
  if (any(is.na(por_gc) || any(is.na(fp_gc)))) {
    return(NA)
  }
  
  distance <- as.vector(distm(fp_gc, por_gc, fun = distHaversine)/ 1000)
  return(distance)
}

# For each manufacturer within shipdf, get mean distance between manu and 
# all of their places of receipt, then get the log of that mean.
getAvgDistPorFp <- function(shipdf, fdaManu, fdaFP, fdaPOR, 
                            fpGC, porGC, featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaManu = col index of manufacturer names within shipdf.
  # fdaManuGC = col index of manuGeoCodes within shipdf.
  # fdaPorGC = col index of porGeoCodes within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("distance manufacturer - place of receipt").
  
  # Create vector that gives, for each element within shipdf, its index within 
  # either porGC$uniPOR or manuGC$uniaddress.
  #por_ids <- unname(sapply(shipdf[[fdaPOR]], function(x) match(x, porGC$uniPOR, incomparables = c("unmatched", NA))))
  #manu_ids <- unname(sapply(shipdf[[fdaManuAddress]], function(x) match(x, manuGC$uniaddress, incomparables = c("unmatched", NA))))
  
  # Calculate distances for each pair of manufacturer address geocodes and 
  # Place of Receipt geocodes.
  distanc <- mapply(function(x, y) 
    getDistPorFp(x, y, porGC, fpGC), 
    shipdf[[fdaPOR]], shipdf[[fdaFP]], USE.NAMES = FALSE)
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  # For each manufacturer, get mean of all of their values found witin 
  # vector "distanc", append the answer to each of that manufacturer's
  # observations within new column "distance manufacturer - place of receipt".
  y <- sapply(ids, function(x) log10(mean(distanc[unlist(x)], na.rm = TRUE)))
  
  # Rewrite all results of "-Inf" to be "0".
  if (any(is.infinite(y))) {
    y[is.infinite(y)] <- 0
  }
  
  featdf$`distance place of receipt - foreign port` <- y
  
  return(featdf)
}


## Transnational Place of Receipt - Foreign Port ----
# Functions: reverseGeocode, getPorCountry, getTransManuPor

# Country lookup function.
por_fp_countrylookup <- function(por, fp, porGC, fpGC) {
  # Params:
  # 
  # Output is
  if (any(is.na(c(por, fp))) || any(c(por, fp) == "unmatched")) {
    return(NA)
  }
  
  porCntry <- porGC[match(por, porGC$uniPOR), ]$porCountry
  fpCntry <- fpGC[match(fp, fpGC$uniFP), ]$fpCountry
  if (any(is.na(c(porCntry, fpCntry)))) {
    return(NA)
  }
  
  if (porCntry == fpCntry) {
    equal <- 0
  } else {
    equal <- 1
  }
  return(equal)
}

# For each manufacturer within featdf, get fraction of shipments
# where the manufacturer and the place of receipt are located in different countries
getTransPorFp <- function(shipdf, fdaManu, fdaPOR, fdaFP, 
                          featdf, featManu, porGC, fpGC) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # manuCountry = col index of manufacturer country within shipdf.
  # fdaCountry = col index of manufacturer countries within shipdf.
  # fdaPorCountry = col index of Place of Receipt country within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("transnational manufacturer - place of receipt")
  
  # Compile binary vector, each value corresponds to 1 row in shipdf, values are
  # zero if the manufacturer country and the Place of Recepit country within a single 
  # row are the same, and one if the two countries are different.
  
  # create binary vector, length == nrow(shipdf), each element indicates whether 
  # the manu country and por country for that row are equal or not.
  trans_por_fp <- mapply(function(x, y) por_fp_countrylookup(x, y, porGC, fpGC), 
                         shipdf[[fdaPOR]], shipdf[[fdaFP]], 
                         USE.NAMES = FALSE)
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  featdf$`transnational place of receipt - foreign port` <- 
    sapply(ids, function(x) mean(trans_por_fp[unlist(x)], na.rm = TRUE))
  
  return(featdf)
}


## Variation of number of consignees across years ----
# Compute the mean num of consignees per year, the standard deviation,
# and finally the coefficient of variation.
getCofV <- function(yr, consign) {
  # Params:
  # yr = numeric vector of years.
  # consign = numeric vector of num of consignees worked with in each year.
  # Output is the coefficient of variation of the inputs.
  if (length(yr) <= 1) {
    return(0)
  }
  avg <- sum(consign) / ((yr[length(yr)] - yr[1]) + 1)
  numerator <- sapply(consign, function(x) (x - avg)^2)
  SD <- sqrt(sum(numerator) / (yr[length(yr)] - yr[1]))
  return(SD / avg)
}


# For each manufacturer in shipdf, get the variation of number of consignees 
# across all years in which that manufacturer was active.
getConsignVar <- function(shipdf, fdaConsign, fdaDate, fdaManu, 
                          featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConsign = col index of the consignee names within shipdf.
  # fdaDate = col index of the ship date within shipdf.
  # fdaManu = col index of the manufacturer names within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("consignee variation").
  print("Now computing Variation of Number of Consignees Across Years...")
  
  # Start by getting all years in which the manufacturer was active
  # i.e. had shipments, and the number of unique consignees worked with in each
  # of those years.
  shipdf_years <- as.numeric(format(shipdf[[fdaDate]], "%Y"))
  
  # Create list of lists that contains, for each manufacturer within featdf, their 
  # indices within shipdf.
  ids <- lapply(
    featdf[[featManu]], function(x) which(shipdf[[fdaManu]] %in% x))
  
  # List of lists, each element is years in which that manufacturer was active, 
  # then for each element get the indices of those years.
  yr <- lapply(ids, function(x) unique(shipdf_years[unlist(x)]))
  yr_ids <- lapply(unique(shipdf_years), function(x) which(shipdf_years %in% x))
  names(yr_ids) <- unique(shipdf_years)
  
  # Create list of lists, each element has same length as it's sister index in yr, 
  # and each is the number of unique consignees that the manu worked with in that year.
  consign <- mapply(function(x, y) 
    sapply(unlist(y), function(z) 
      length(
        unique(
          shipdf[intersect(
            x, unname(unlist(yr_ids[which(names(yr_ids) %in% 
                                            as.character(z))]))), ][[fdaConsign]]))), ids, yr, USE.NAMES = FALSE)
  
  # Edit the year and num consignee vectors to fill in any data missing due to 
  # inactivity.
  # e.g. yr = "2008, 2009, 2012" and consign = "6, 10, 8" would become
  # yr = "2008, 2009, 2010, 2011, 2012" and consign = "6, 10, 0, 0, 8"
  for (i in seq_len(length(yr))) {
    if (length(yr[[i]]) <= 1) {
      next
    }
    consignfull <- vector()
    yrfull <- yr[[i]][1]:yr[[i]][length(yr[[i]])]
    for (k in seq_len(length(yrfull))) {
      if (any(yr[[i]] == yrfull[k])) {
        consignfull[k] <- consign[[i]][which(yr[[i]] == yrfull[k])]
      } else {
        consignfull[k] <- 0
      }
    }
    yr[[i]] <- yrfull
    consign[[i]] <- consignfull
  }
  
  # Compute the mean num of consignees per year, the standard deviation,
  # and finally the coefficient of variation.
  featdf$`consignee variation` <- mapply(function(x, y) 
    getCofV(unlist(x), unlist(y)), yr, consign, USE.NAMES = FALSE)
  
  return(featdf)
}


## Variation of number of shipments across years ----
# For each manufacturer in shipdf, get the variation of number of shipments 
# across all years in which that manufacturer was active.

getShipmentsVar <- function(shipdf, fdaDate, fdaManu, featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaDate = col index of the ship date within shipdf.
  # fdaManu = col index of the manufacturer names within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("shipment variation")
  print("Now computing Variation of Number of Shipments Across Years...")
  
  # Start by getting all years in which the manufacturer was active
  # i.e. had shipments, and the number of shipments in each of those years.
  featdf$`shipment variation` <- NA
  shipdf$year <- as.numeric(format(shipdf[[fdaDate]], "%Y"))
  for (i in featdf[[featManu]]) {
    yr <- unique(shipdf[shipdf[[fdaManu]] == i, ]$year)
    nship <- vector()
    for (n in yr) {
      nship <- c(nship, nrow(shipdf[shipdf[[fdaManu]] == i & shipdf$year == n, ]))
    }
    # Edit the year and num shipment vectors to fill in any data missing due to 
    # inactivity.
    # e.g. yr = "2008, 2009, 2012" and nship = "6, 10, 8" would become
    # yr = "2008, 2009, 2010, 2011, 2012" and nship = "6, 10, 0, 0, 8"
    if (length(yr) > 1) {
      yrfull <- yr[1]:yr[length(yr)]
      nshipfull <- vector()
      for (k in seq_len(length(yrfull))) {
        if (any(yr == yrfull[k])) {
          id <- grep(yrfull[k], yr)
          nshipfull[k] <- nship[id]
        } else {
          nshipfull[k] <- 0
        }
      }
    } else {
      yrfull <- yr
      nshipfull <- nship
    }
    # Last, compute the mean num of shipments per year, the standard deviation,
    # and the coefficient of variation (CoV gets recorded to featdf).
    if (length(yrfull) == 1) {
      featdf[featdf[[featManu]] == i, ]$`shipment variation` <- 0
    } else {
      avg <- sum(nshipfull) / ((yrfull[length(yrfull)] - yrfull[1]) + 1)
      numerator <- vector()
      for (t in seq_len(length(yrfull))) {
        numerator <- c(numerator, (nshipfull[t] - avg)^2)
      }
      SD <- sqrt(sum(numerator) / (yrfull[length(yrfull)] - yrfull[1]) + 10e-6)
      featdf[featdf[[featManu]] == i, ]$`shipment variation` <- SD / avg
    }
  }
  return(featdf)
}


## Is Manufacturer in One of these Countries ----
# For each manufacturer, indicate whether they are located in any of these countries:
# Bangladesh, China, India, Indonesia, Vietnam, Malaysia, Thailand.

specificCountries <- function(shipdf, fdaManu, fdaCountry, featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaManu = col index of the manufacturer names within shipdf.
  # fdaCountry = col index of manufacturer countries within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("specific country")
  print("Now computing Is Manufacturer in One of these Countries...")
  
  featdf$`specific country` <- NA
  for (i in seq_len(nrow(featdf))) {
    x <- shipdf[shipdf[[fdaManu]] == featdf[[featManu]][i], ][[fdaCountry]][1]
    if (x %in% c("Bangladesh", "China", "India", "Indonesia", "Vietnam", "Malaysia", "Thailand")) {
      featdf$`specific country`[i] <- x
    } else {
      featdf$`specific country`[i] <- "other"
    }
  }
  return(featdf)
}


## Dispersion of Total Shipment Weight per Consignee ----
# For each manufacturer, get the dispersion of the total shipment weight per 
# consignee that the manufacturer has shipped to within the shipdf.
# This dispersion calculation is a measure of entropy.

getConsigneeDispersion <- function(shipdf, fdaConsign, fdaManu, fdaKG, 
                                   featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConsign = col index of the consignee names within shipdf.
  # fdaManu = col index of the manufacturer names within shipdf.
  # fdaKG = col index of the shipment weights (KG) within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("consignee dispersion")
  print("Now computing Dispersion of Total Shipment Weight per Consignee...")
  
  featdf$`consignee dispersion` <- NA
  for (i in seq_len(nrow(featdf))) {
    if (featdf$numConsignees[i] == 0) {
      featdf$`consignee dispersion`[i] <- 0
      next
    }
    subship <- shipdf[shipdf[[fdaManu]] == featdf[[featManu]][i] & 
                        !is.na(shipdf[[fdaManu]]), c(fdaManu, fdaConsign, fdaKG)]
    subship <- subship[complete.cases(subship), ]
    normweight <- vector()
    for (n in unique(subship[[2]])) {
      x <- sum(subship[subship[[2]] == n, ][[3]]) / sum(subship[[3]])
      normweight <- c(normweight, x*log(x))
    }
    featdf$`consignee dispersion`[i] <- -sum(normweight)
  }
  return(featdf)
}


## Fraction of Shipments to "Inexperienced" Consignees ----
# For each manufacturer, get fraction of shipments sent to "inexperienced" consignees.

getConsigneeExperience <- function(shipdf, fdaConsign, fdaManu, fdaKG, featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaConsign = col index of the consignee names within shipdf.
  # fdaManu = col index of the manufacturer names within shipdf.
  # fdaKG = col index of the shipment weights (KG) within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("shipments to inexperienced consignees")
  print("Now computing Fraction of Shipments to 'Inexperienced' Consignees...")
  
  featdf$`shipments to inexperienced consignees` <- NA
  uniconsign <- unique(shipdf[[fdaConsign]][!is.na(shipdf[[fdaConsign]])])
  weights <- unname(sapply(
    uniconsign, function(x) sum(shipdf[shipdf[[fdaConsign]] == x & 
                                         !is.na(shipdf[[fdaConsign]]), ][[fdaKG]], na.rm = TRUE)))
  inexp <- unname(ifelse(weights > quantile(weights, 0.10), 0, 1))
  id <- unname(sapply(shipdf[[fdaConsign]], function(x) match(x, uniconsign)))
  inexpConsign <- unname(sapply(id, function(x) inexp[x]))
  for (i in seq_len(nrow(featdf))) {
    id <- grep(featdf[[featManu]][i], shipdf[[fdaManu]], fixed = TRUE)
    featdf$`shipments to inexperienced consignees`[i] <- sum(inexpConsign[id]) / length(id)
  }
  return(featdf)
}


## Fraction of Sampled Shipments & Number of Sampled Shipments ----
# For each manufacturer, get their fraction of shipments sampled within shipdf.

getNumSampled <- function(shipdf, fdaManu, fdaSamp, featdf, featManu) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaManu = col index of the manufacturer names within shipdf.
  # fdaSamp = col index of "Intermediate Activity Number" within shipdf.
  # featdf = features dataframe.
  # featManu = col index of manufacturer names within featdf.
  # Output is featdf with 1 new column ("numSampled").
  print("Now computing Fraction of Sampled Shipments & Number of Sampled Shipments...")
  
  featdf$numSampled <- NA
  featdf$`fraction of sampled shipments` <- NA
  sampled <- ifelse(!is.na(shipdf[[fdaSamp]]), 1, 0)
  for (i in seq_len(nrow(featdf))) {
    id <- grep(featdf[[featManu]][i], shipdf[[fdaManu]], fixed = TRUE)
    featdf$`fraction of sampled shipments`[i] <- sum(sampled[id]) / length(id)
    featdf$numSampled[i] <- sum(sampled[id])
  }
  return(featdf)
}


## intentional vs unintentional ----
# Label refused shipments as either intentional or unintentional (in reference 
# to their adulteration type).
# Functions: refPrep, getRefusalCats, getIntent, getIntentFractions

refPrep <- function(ref, refDate) {
  # Params:
  # ref = dataframe, fda refusal dataset.
  # refDate = col index of dates within ref.
  # Output is ref with transformations applied.
  print("Now computing Intentional vs Unintentional...")
  
  ref[[refDate]] <- as.Date(ref[[refDate]], format = "%d-%b-%y")
  ref <- ref[order(ref[[refDate]]), ]
  return(ref)
}

getRefusalCats <- function(shipdf, fdaManu, fdaDate, fdaRefuse, 
                            ref, refManu, refDate, refCode) {
  # Params:
  # shipdf = dataframe, FDA shipping data.
  # fdaManu = col index of manufacturer names within shipdf.
  # fdaDate = col index of dates within shipdf.
  # fdaRefuse = col index of refusal info within shipdf.
  # ref = dataframe, fda refusal dataset.
  # refManu = col index of manufacturer names within ref.
  # refDate = col index of dates within ref.
  # refCode = col index of refusal codes within ref.
  # Output is vector of length nrow(shipdf), each element contains the refusal
  # category for any shipments within shipdf that were refused.
  
  # Data Prep.
  ref <- refPrep(ref, refDate)
  
  # Create Variables.
  refused <- ifelse(!is.na(shipdf[[fdaRefuse]]) & 
                      (shipdf[[fdaRefuse]] == 154 | shipdf[[fdaRefuse]] == 155), 1, 0)
  refCats <- vector()
  
  # Start the for loop
  for (i in seq_len(length(refused))) {
    if (refused[i] == 1) {
      if (!is.na(shipdf[[fdaFEI]][i]) && 
          any(grepl(shipdf[[fdaFEI]][i], ref[[refFEI]], fixed = TRUE))) {
        id <- grep(shipdf[[fdaFEI]][i], ref[[refFEI]], fixed = TRUE)
        refID <- ref[id, c(refDate, refCode)]
        pre <- refID[[1]] >= (shipdf[[fdaDate]][i] - 5)
        post <- refID[[1]] <= (shipdf[[fdaDate]][i] + 100)
        if (any(rowSums(cbind(pre, post)) == 2)) {
          id2 <- grep(2, rowSums(cbind(pre, post)))[1]
          refCats <- c(refCats, refID[[2]][id2])
        } else {
          refCats <- c(refCats, NA)
        }
      } else {
        refCats <- c(refCats, NA)
      }
    } else {
      refCats <- c(refCats, NA)
    }
  }
  return(refCats)
}


getIntent <- function (refCats) {
  # Params:
  # refCats = vector of refusal codes generated from shipdf.
  # Output is a dataframe of 2 binary variables, one for intentional,
  # one for unintentional.
  intent <- ifelse(is.na(refCats), 0, ifelse(grepl("2860|3220|238|482|11|218", refCats), 1, 0))
  unintent <- ifelse(is.na(refCats), 0, ifelse(grepl("^9$|9, | 9|249", refCats), 1, 0))
  intentdf <- data_frame(intentional = intent, unintentional = unintent)
  return(intentdf)
}

getIntentFractions <- function(shipdf, fdaManu, fdaDate, fdaRefuse, fdaSamp, 
                               featdf, featSamp, featNshipments, featManu, 
                               ref, refDate, refManu, refCode) {
  # Params:
  # shipdf = dataframe, the FDA shipping data.
  # fdaManu = col index of manufacturer names within shipdf.
  # fdaDate = col index of dates within shipdf.
  # fdaRefuse = col index of refusal info within shipdf.
  # fdaSamp = col index of sampling info within shipdf.
  # featdf = features dataframe.
  # featSamp = col index of fraction of sampled shipments within featdf.
  # featNshipments = col index of number of shipments within featdf.
  # featManu = col index of manufacturer names within featdf.
  # ref = dataframe, fda refusals dataset.
  # refDate = col index of dates within ref.
  # refManu = col index of manufacturer names within ref.
  # refCode = col index of refusal codes within ref.
  # Output featdf with 2 new columns ("fraction of sampled refused and intentional" & 
  # "fraction of sampled refused and unintentional")
  
  # Do matching of shipping records and refusal records.
  refCats <- getRefusalCats(shipdf, fdaManu, fdaDate, fdaRefuse, 
                            ref, refManu, refDate, refCode)
  
  # Label the positive matches as being either intentional or unintentional.
  intentdf <- getIntent(refCats)
  
  featdf$`fraction of sampled refused and intentional` <- NA
  featdf$`fraction of sampled refused and unintentional` <- NA
  for (i in seq_len(nrow(featdf))) {
    numSamp <- featdf[[featNshipments]][i] * featdf[[featSamp]][i]
    id <- grep(featdf[[featManu]][i], shipdf[[fdaManu]], fixed = TRUE)
    sumIntent <- sum(intentdf[id, ][[1]])
    sumUnintent <- sum(intentdf[id, ][[2]])
    if (sumIntent == 0 || numSamp == 0) {
      featdf$`fraction of sampled refused and intentional`[i] <- 0
    } else {
      featdf$`fraction of sampled refused and intentional`[i] <- sumIntent / numSamp
    }
    if (sumUnintent == 0 || numSamp == 0) {
      featdf$`fraction of sampled refused and unintentional`[i] <- 0
    } else {
      featdf$`fraction of sampled refused and unintentional`[i] <- sumUnintent / numSamp
    }
  }
  return(featdf)
}