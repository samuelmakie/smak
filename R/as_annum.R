#' as_annum
#'
#' @param date
#' @param from
#' @param sep
#' @param format
#'
#' @return
#' @export
#'
#' @examples
as_annum <- function(date, from = "09-01", sep = " ", format = "%Y"){
  annum_ending <- as.numeric(format(date, format)) + as.integer(date >= as.Date(paste0(format(date, "%Y"), "-", from), format = "%Y-%m-%d"))
  output <- paste0(annum_ending - 1, sep, annum_ending)
  output[which(is.na(date))] <- NA
  return(output)
}

