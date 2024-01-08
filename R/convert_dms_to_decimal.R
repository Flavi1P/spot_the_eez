# Function to convert DMS to decimal degrees
convert_dms_to_decimal <- function(dms) {
  matches <- regmatches(dms, gregexpr("\\d+\\.\\d+|\\d+", dms, perl = TRUE))
  
  if(length(matches[[1]]) > 0) {
    numeric_values <- as.numeric(matches[[1]])
    
    degrees <- numeric_values[1]
    minutes <- ifelse(length(numeric_values) > 1, numeric_values[2], 0)
    seconds <- ifelse(length(numeric_values) > 2, numeric_values[3], 0)
    
    decimal_degrees <- degrees + (minutes / 60) + (seconds / 3600)
    
    # if (grepl("S", dms, fixed = TRUE) | grepl("W", dms, fixed = TRUE)) {
    #    decimal_degrees <- -decimal_degrees
    # }
    
    return(decimal_degrees)
  } else {
    return(NA)  # or any other appropriate value when no numeric match is found
  }
}