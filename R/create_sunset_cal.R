#' Create a sunset calendar
#'
#' This function creates a .CSV of sunset appointments--with a user-specified location--that can be imported into Google Calendar. 
#' @param date Date at which you want the calendar to start, in yyyy/mm/dd format.
#' @param lat Latitude of location (for sunset time calculation)
#' @param long Longitude of location (for sunset time calculation, will be negative for continental US)
#' @param timezone Timezone of location (for sunset time calculation). 
#' @param num.days Number of days you want sunset appointments for.
#' @param file Filename for outputted .CSV file (to be uploaded to Google Calendar).
#' @param location Location of sunset appointment. Will be input into Google Calendar event as the event location.
#' @importFrom StreamMetabolism sunrise.set
#' @export
#' @examples \dontrun{
#' create_sunset_cal(location = "40.7127, -74.0059")
#'}
#' 

create_sunset_cal <- function(date="2014/01/01", 
                              lat = 40.7127, 
                              long = -74.0059,
                              timezone = "America/New_York",
                              num.days = 365,
                              file="sunset.csv",
                              location = "Brooklyn Heights Promenade, Brooklyn, NY 11201"){
  
  location <- gsub(",", "", location)
  
  dates <- seq(
    as.Date(date), 
    by = "day", 
    length.out = num.days
  )
  
  sunset_times <- sunrise.set(
    lat = lat, 
    long = long, 
    date = date, 
    timezone = timezone,
    num.days = num.days
  )$sunset
  
  nms <- c(
    'Subject',
    'Start Date',
    'Start Time',
    'End Date',
    'End Time',
    'All Day Event',
    'Description',
    'Location',
    'Private'
  )
  mat <- matrix(
    nrow = length(dates),
    ncol = length(nms)
  )
  mat <- data.frame(mat)
  colnames(mat) <- nms
  
  mat$Subject <- "Sunset"
  mat$"Start Date" <- dates
  mat$"End Date" <- dates
  mat$"All Day Event" <- "False"
  mat$Description <- "Sunset Calendar"
  mat$Location <- location
  mat$Private <- "False"
  
  starts <- strftime(sunset_times, format="%H:%M:%S %p")
  ends <- strftime(sunset_times+60*30, format="%H:%M:%S %p")
  mat$"Start Time" <- starts
  mat$"End Time" <- ends
  
  write.csv(
    mat,
    file=file,
    quote=FALSE,
    row.names=FALSE
  )
  
}
