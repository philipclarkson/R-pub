# Geohashing functions. Implements the algorithm described in
# http://en.wikipedia.org/wiki/Geohash

BASE32 <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "b", "c", "d",
            "e", "f", "g", "h", "j", "k", "m", "n", "p", "q", "r", "s", "t",
            "u", "v", "w", "x", "y", "z")
BITS <- c(16, 8, 4, 2, 1)

EncodeGeohash <- function(latitude, longitude, precision = 12) {

    # Returns a geohash of a lat/long pair.
    # Args:
    #   latitude: Numeric between -90 and 90 giving point's latitude.
    #   longitude: Numeric between -180 and 180 giving point's longitude.
    #   precision: The number of characters in the required geohash.
    # Returns:
    #   A string with the geohash of the lat/long pair.
    
    stopifnot(length(latitude) == 1,
              length(longitude) == 1,
              precision > 0,
              latitude > -90,
              latitude < 90,
              longitude > -180,
              longitude < 180)
    
    lat <- c(-90, 90)
    lon <- c(-180, 180)
    is.even <- TRUE
    bit <- 1
    ch <- 0
    geohash <- ""
    
    while (nchar(geohash) < precision) {
        if (is.even) {
            mid <- mean(lon)
            if (longitude > mid) {
                ch <- bitwOr(ch, BITS[bit])
                lon[1] <- mid
            } else {
                lon[2] <- mid
            }
        } else {
            mid <- mean(lat)
            if (latitude > mid) {
                ch <- bitwOr(ch, BITS[bit])
                lat[1] <- mid
            } else {
                lat[2] <- mid
            }
        }
        is.even <- !is.even
        if (bit < 5) {
            bit <- bit + 1
        } else {
            geohash <- paste0(geohash, BASE32[ch + 1])
            bit <- 1
            ch <- 0
        }
    }
t
    return(geohash)
    
}

DecodeGeohash <- function(geohash) {

    # Computes the lat/long of the input geohash.
    # Args:
    #   geohash: A string (containing only the characters in BASE32).
    # Returns:
    #   A list with the elements:
    #      lat.range: A vector of length 2, contains the min and max latitude.
    #      lon.range: A vector of length 2, contains the min and max longitude.
    #      lat: The mid-point of lat.range.
    #      lon: The mid-point of lon.range.
    
    stopifnot(inherits(geohash, "character"),
              length(geohash) == 1,
              all(strsplit(geohash, "")[[1]] %in% BASE32))
    
    is.even <- TRUE
    lat <- c(-90, 90)
    lon <- c(-180, 180)

    for (i in seq_len(nchar(geohash))) {
        cd <- which(BASE32 == substr(geohash, i, i)) - 1
        for (j in 1:5) {

            if (is.even) {
                if (bitwAnd(cd, BITS[j])) {
                    lon[1] <- mean(lon)
                } else {
                    lon[2] <- mean(lon)
                }
            } else {
                if (bitwAnd(cd, BITS[j])) {
                    lat[1] <- mean(lat)
                } else {
                    lat[2] <- mean(lat)
                }
            }
            is.even <- !is.even
        }
    }

    return(list(lat.range = lat, lon.range = lon,
                lat = mean(lat), lon = mean(lon)))
                   
    
}

