library(testthat)

input.string <- "
lat,lon,geohash,precision
39.92324,116.3906,wx4g0,5
45.37,-121.7,c216ne,6
47.6062095,-122.3320708,c23nb62w20sth,13
35.6894875,139.6917064,xn774c06kdtve,13
-33.8671390,151.2071140,r3gx2f9tt5sne,13
51.5001524,-0.1262362,gcpuvpk44kprq,13
"

dat <- read.csv(textConnection(input.string), stringsAsFactors = FALSE)

test_that("Known points give expected geohash", {
    for (i in seq_len(nrow(dat))) {
        expect_equal(EncodeGeohash(dat[i, "lat"], dat[i, "lon"],
                                  precision = dat[i, "precision"]),
                    dat[i, "geohash"])
        decode.output <- DecodeGeohash(dat[i, "geohash"])
        expect_true(dat[i, "lat"] > decode.output$lat.range[1])
        expect_true(dat[i, "lat"] < decode.output$lat.range[2])
        expect_true(dat[i, "lon"] > decode.output$lon.range[1])
        expect_true(dat[i, "lon"] < decode.output$lon.range[2])
    }
})

test_that("Bad values fail", {
    expect_error(EncodeGeohash("not a number", 0))
    expect_error(EncodeGeohash(-95, 0))
    expect_error(EncodeGeohash(c(0, 0), c(0, 0)))
    expect_error(EncodeGeohash(NA, 0))
    expect_error(EncodeGeohash(0, 190))
    expect_error(EncodeGeohash(0))
    expect_error(DecodeGeohash(13))
    expect_error(DecodeGeohash("BC99"))
    expect_error(DecodeGeohash("o"))
    expect_error(DecodeGeohash("a"))
    expect_error(DecodeGeohash("i"))
    expect_error(DecodeGeohash("l"))
    expect_error(DecodeGeohash(c("bc", "cd")))
})
