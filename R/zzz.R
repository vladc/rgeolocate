rgeo_env <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  maxmind_tags <- c("continent_name", "country_name", "country_code", "region_name",
                    "city_name", "timezone", "connection", "city_geoname_id", "latitude", "longitude",
                    "isp", "organization", "asn", "aso", "postcode")
  ipinfo_tags <- c("hostname", "city", "region", "country",
                   "loc", "org", "postal", "phone")
  
  ip2loc_tags <- c("country_code", "country_name", "region", "city", "isp", "lat", 
                   "long", "domain", "zip_code", "timezone", "netspeed", "international_code", 
                   "area_code", "station_code", "station_name", "mcc", "mnc", "mobile_brand", 
                   "elevation", "usage_type")
  ipapi_tags  <- c("status"=16384,"message"=32768,"continent"=1048576,"continentCode"=2097152,
                   "country"=1,"countryCode"=2,"region"=4,"regionName"=8,"city"=16,"district"=524288,
		   "zip"=32,"lat"=64,"lon"=128,"timezone"=256,"currency"=8388608,"isp"=512,"org"=1024,
		   "as"=2048,"asname"=4194304,"mobile"=65536,"proxy"=131072,"query"=8192)

  assign("maxmind_tags", maxmind_tags, envir = rgeo_env)
  assign("ipinfo_tags", ipinfo_tags, envir = rgeo_env)
  assign("ip2loc_tags", ip2loc_tags, envir = rgeo_env)
  assign("ipapi_tags", ipapi_tags, envir = rgeo_env)
}
