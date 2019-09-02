ip_api_single <- function(ip, sleep, key, ssl, fields){
  if(sleep){
    Sys.sleep(0.40)
  }
  if(is.null(key)){
    url <- paste0("http://ip-api.com/json/", ip, "?fields=", fields)
  }else{
    url <- paste0(ifelse(isTRUE(ssl), "https://", "http://"), "pro.ip-api.com/json/", ip, "?fields=", fields, "&key=", key)
  }

  result <- httr::GET(url, user_agent("rgeolocate - https://github.com/Ironholds/rgeolocate"))
  if(result$status > 300){
    return("Error")
  }
  
  parsed_results <- httr::content(result, as = "parsed", type = "application/json")
  return(unlist(parsed_results))
}

ip_api_batch <- function(ips, sleep, key, ssl, fields){
  batch <- c()
  batches <- c()
  for(ip in ips){
    # up to 100 objects per HTTP request
    if(length(batch) == 100){
      batches = append(batches, list(batch))
      batch <- c()
    }
    batch = append(batch, list(list(query = ip)))
  }
  if(length(batch) > 0){
    batches = append(batches, list(batch))
  }
  if(is.null(key)){
    url <- paste0("http://ip-api.com/batch?fields=", fields)
  }else{
    url <- paste0(ifelse(isTRUE(ssl), "https://", "http://"), "pro.ip-api.com/batch?fields=", fields, "&key=", key)
  }
  results <- list()
  for(req in batches){
    if(sleep){
      Sys.sleep(0.40)
    }
    result = httr::POST(url, body = req, encode = "json", user_agent("rgeolocate - https://github.com/Ironholds/rgeolocate"))
    parsed_results <- httr::content(result, as = "parsed", type = "application/json")
    results = append(results, parsed_results)
  }
  return(lapply(results, unlist))
}
#'@title Geolocate IP Addresses Through ip-api.com
#'@description \code{ip_api} consumes a vector of IP addresses
#'and geolocates them via \href{http://ip-api.com}{ip-api.com}.
#'
#'@param ip_addresses a character vector of IP addresses
#'
#'@param as_data_frame whether to return the results as a data.frame or not.
#'Set to TRUE by default.
#'
#'@param delay whether or not to delay each request by 400ms. ip-api.com has a
#'maximum threshold of 150 requests a minute; if you're parallelising calls, you
#'might run into this. \code{delay} allows you to set a delay between requests, taking
#'advantage of parallelisation while avoiding running into this threshold. Set to
#'FALSE by default
#'
#'@param key - optionally, an API key. See https://signup.ip-api.com/ for details
#'
#'Set to NULL by default.
#'
#'@param ssl whether to return use SSL (only when used with a key).
#'Set to TRUE by default.
#'
#'@param fields fields the fields you want to retrieve - a vector of any combination
#'from http://ip-api.com/docs/api:json
#'Default: c("status","message","country","countryCode","region","regionName","city",
#'"zip","lat","lon","timezone","isp","org","as","query")
#'
#'@return either a data.frame or a list of vectors. If an IP cannot be geolocated, it
#'will provide an error message: see the examples for field names and examples of each
#'possible output.
#'
#'@seealso \code{\link{ip_info}} and \code{\link{db_ip}} for other
#'online geolocation APIs.
#'
#'@examples
#'\dontrun{
#'#Valid, data.frame output
#'result <- ip_api("2607:FB90:426:DC1D:CFC4:4875:8BC2:4D93")
#'
#'#Invalid, data.frame output
#'result <- ip_api("argh")
#'
#'#Valid list output
#'result <- ip_api("2607:FB90:426:DC1D:CFC4:4875:8BC2:4D93", as_data_frame = FALSE)
#'
#'#Invalid list output
#'result <- ip_api("argh", as_data_frame = FALSE)
#'}
#'@export
ip_api <- function(ip_addresses, as_data_frame = TRUE, delay = FALSE, key = NULL, ssl = TRUE, 
            fields = c("status","message","country","countryCode","region","regionName","city",
            "zip","lat","lon","timezone","isp","org","as","query")){

  valid_fields <- (fields %in% names(rgeo_env$ipapi_tags))
  if(!all(valid_fields)){
    warning("Some field names you have provided are not supported and no data will be retrieved for them. \nThe
            unsupported fields are: ", paste(fields[!valid_fields], collapse = ", "))
    fields <- fields[valid_fields]
  }
  # calculate a decimal value for the fields, based on http://ip-api.com/docs/api:json
  decimalFields = 0
  for(i in fields){
    decimalFields = decimalFields + rgeo_env$ipapi_tags[i]
  }

  if(length(ip_addresses) > 1){
    results <- ip_api_batch(ip_addresses, delay, key, ssl, decimalFields)
  }else{
    results <- lapply(ip_addresses, ip_api_single, delay, key, ssl, decimalFields)
  }

  if(as_data_frame){
    df <- data.frame(stringsAsFactors = FALSE, matrix(ncol = length(fields), nrow = length(results)))
    colnames(df) <- fields
    rows = 1
    for(result in results){
      for(field in fields){
        if(is.na(result[field]) || result[field] == "") {
	  df[rows,][field] = NA
	}else{
          df[rows,][field] = result[field]
	}
      }
      rows = rows + 1
    }
    return(df)
  }
  return(results)
}
