context("Test MaxMind.")
test_that("non-existent files are detected", {
  expect_that(maxmind("foo","[@8"), throws_error("could not be opened"))
})

test_that("non-existent IPs are detected", {
  file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeoip")
  results <- maxmind("foo",file)
  expect_that(results[1,1], equals("Unknown"))
})

test_that("IPs without entries are detected", {
  file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeoip")
  results <- maxmind("10.68.16.31",file)
  expect_that(results[1,1], equals("Unknown"))
})

test_that("A df of the right dimensions is returned", {
  file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeoip")
  results <- maxmind("196.200.60.51",file)
  expect_that(nrow(results), equals(1))
  expect_that(ncol(results), equals(7))
  expect_that(is.data.frame(results), equals(TRUE))
  expect_that(names(results), equals(c("continent_name", "country_name", "country_code",
                                       "region_name", "city_name", "timezone",
                                       "connection_type")))
})

test_that("A df of the right values is returned", {
  file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeoip")
  results <- maxmind("196.200.60.51",file)
  expect_that(results$continent_name[1], equals("Africa"))
  expect_that(results$country_code[1], equals("ML"))
  expect_that(results$country_name[1],equals("Mali"))
})