#'@title Geolocate IP Addresses through MaxMind Databases
#'@description MaxMind does a set of proprietary geolocation databases
#'- they're pretty accurate! \code{maxmind} provides a connector to
#'MaxMind services.
#'
#'@param ips a character vector of IP addresses (IPv4 and IPv6 both work)
#'
#'@param file the full path to the .mmdb file you want to query. 
#'
#'@details
#'\code{geolookup} uses the \href{http:#dev.maxmind.com/geoip/geoip2/downloadable/}{MaxMind GeoIP2 databases}
#'to geolocate IP addresses, retrieving any of the data listed in \code{fields}. Different fields are
#'appropriate for different provided files; the connection type databases, for example, contain connection
#'types and nothing else, while the city- and country-level files don't contain connection types at all.
#'
#'In the event that the file provided does not have the field you have requested (or the IP address does
#'not have an entry for that field), the string "Unknown" will be returned instead. In the event that the IP
#'address doesn't have an entry in the file at all, "Unknown" will be returned for every field.
#'
#'@examples
#'file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
#'results <- maxmind("196.200.60.51", file)
#'@export
maxmind <- function(ips, file){
  maxmind_(ips, file)
}