#' as_annum
#'
#' @param date A list of date(s)
#' @param from The first day of an annum in the format "mm-dd"
#' @param sep A separator string. Defaults to " ".
#' @param format Format of the year to be returned. Defaults to "yyyy". Together with the sep argument, the year returned would be in the format "yyyy yyyy".
#'
#' @return A vector of character strings indicating the year the list of dates belong.
#' @export
#'
#' @examples
as_annum <- function(date, from = "09-01", sep = " ", format = "%Y"){
  annum_ending <- as.numeric(format(date, format)) + as.integer(date >= as.Date(paste0(format(date, "%Y"), "-", from), format = "%Y-%m-%d"))
  output <- paste0(annum_ending - 1, sep, annum_ending)
  output[which(is.na(date))] <- NA
  return(output)
}

